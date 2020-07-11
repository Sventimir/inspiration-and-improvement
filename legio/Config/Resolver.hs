{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Config.Resolver (
    Resolver,
    loadResolver,
    resolve
) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (manyTill)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Compose (Replaceable(..), Splitable(..), composeState, partialState)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Bifunctor (Bifunctor(..))
import Data.Card (Card(..), cardParser, count)
import Data.CardSet (CardSet, deck, discard, hand)
import Data.Either.Combinators (mapLeft)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Split (Split, moveLeft, moveRight, removeLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Data.Tuple.Extra (secondM)

import Language.Resolvers.Compiler (compile, prettyError)
import Language.Resolvers.Expr (Expr(..), eval)
import Language.Resolvers.Lexer (Parser, body, lexeme, symbol)
import Language.Resolvers.Types (EType(..))
import Language.Resolvers.Unchecked (UExpr, UExprConstr(..))

import Prelude hiding (takeWhile, readFile)

import Text.Megaparsec (ParsecT, runParserT)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (errorBundlePretty)

import System.IO (FilePath, IOMode(..))

import UI.Player (Enemy(..), Player(..), cardSet, damage, legio)


newtype PlayerResolution = PRes (Player, Enemy)

instance Splitable (Player, Player) PlayerResolution PlayerResolution where
    split (a, b) = (PRes (a, enemyFromPlayer b), PRes (b, enemyFromPlayer a))
    fuse (PRes (a, _)) (PRes (b, _)) = (a, b)

newtype Resolution = Res (Split Int, Enemy)

instance Replaceable PlayerResolution Resolution where
    extract (PRes (p, enemy)) = Res (legio p, enemy)
    replace (Res (l, _)) (PRes (p, enemy)) = PRes (replace l p, enemy)

type Resolver = Map (Card, Card) (Expr Resolution ())


resolve :: forall m . Monad m => Resolver -> Card -> Card -> StateT (Player, Player) m ()
resolve resolver playerCard enemyCard = do
    let plCmd = fromMaybe (Const ()) $ Map.lookup (playerCard, enemyCard) resolver
        enCmd = fromMaybe (Const ()) $ Map.lookup (enemyCard, playerCard) resolver
    (player, enemy) <- get
    composeState
        (partialState $ eval plCmd :: StateT PlayerResolution m ())
        (partialState $ eval enCmd :: StateT PlayerResolution m ())


enemyFromPlayer :: Player -> Enemy
enemyFromPlayer (Player p) =
    let cset = cardSet p
        discardCount = count $ discard cset
        totalCount = count (discard cset ++ hand cset ++ deck cset)
    in
    Enemy (damage p) (legio p) totalCount discardCount

loadResolver :: FilePath -> EitherT Text IO Resolver
loadResolver filename = do
    text <- liftIO $ readFile filename
    result <- liftIO $ Megaparsec.runParserT resolverParser filename text
    resolvers <- hoistEither $ mapLeft (Text.pack . errorBundlePretty) result
    hoistEither . bimap (prettyError text) Map.fromList . sequence $ fmap (secondM compile) resolvers

resolverParser :: Parser IO [((Card, Card), UExpr Resolution)]
resolverParser = flip manyTill Megaparsec.eof $ do
    leftCard <- lexeme cardParser
    symbol ":"
    rightCard <- lexeme cardParser
    body <- body primitives
    return ((leftCard, rightCard), body)


asgnKill :: Int -> Resolution -> Resolution
asgnKill k (Res (split, enemy)) = Res (removeLeft k split, enemy)

asgnRout :: Int -> Resolution -> Resolution
asgnRout k (Res (split, enemy)) = Res (moveRight k split, enemy)

asgnRestore :: Int -> Resolution -> Resolution
asgnRestore k (Res (split, enemy)) = Res (moveLeft k split, enemy)

primitives :: Map Text (UExprConstr Resolution)
primitives = Map.fromList [
        ("damage",  CVar EInt $ \(Res (_, Enemy d _ _ _)) -> d),
        ("kill",    CConst (EFun EInt EAlter) asgnKill),
        ("rout",    CConst (EFun EInt EAlter) asgnRout),
        ("rally",   CConst (EFun EInt EAlter) asgnRestore),
        ("pass",    CConst EUnit ())
    ]
