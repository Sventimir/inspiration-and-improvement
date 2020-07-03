{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Config.Resolver (
    Resolver,
    loadResolver,
    resolve
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Compose (Replaceable(..), Splitable(..), composeState, partialState)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Attoparsec.Text (Parser, choice, decimal, eitherResult, many', many1',
        parseWith, skip, skipSpace, string)
import Data.Card (Card(..), cardParser, count)
import Data.CardSet (CardSet, deck, discard, hand)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Split (Split, moveLeft, moveRight, removeLeft)
import Data.Text.IO (hGetChunk)

import Language.Resolvers.Expr (Expr(..), eval)

import System.IO (FilePath, IOMode(..), openFile)

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

loadResolver :: FilePath -> EitherT String IO Resolver
loadResolver filename = do
    file <- liftIO $ openFile filename ReadMode
    c <- liftIO $ hGetChunk file
    r <- liftIO $ parseWith (hGetChunk file) resolverParser c
    hoistEither $ eitherResult r

resolverParser :: Parser Resolver
resolverParser = fmap Map.fromList . many1' $ do
    leftCard <- cardParser
    skipSpace
    skip (== ':') <|> fail "Chuj ci w dłoń"
    skipSpace
    rightCard <- cardParser
    skipSpace
    body <- bodyParser
    skipSpace
    return ((leftCard, rightCard), body)

bodyParser :: Parser (Expr Resolution ())
bodyParser = do
    skip (== '{')
    skipSpace
    exprs <- many1' exprParser
    skipSpace
    skip (== '}')
    skipSpace
    return $ seqExpr exprs

seqExpr :: [Expr Resolution ()] -> Expr Resolution ()
seqExpr [] = Const ()
seqExpr [e] = e
seqExpr (e : es) = Seq e $ seqExpr es

exprParser :: Parser (Expr Resolution ())
exprParser = do
    cmd <- choice [
            symbol "pass" >> return (Const ()),
            symbol "kill" >> fmap (Assign . App (Const asgnKill)) parseArg,
            symbol "rout" >> fmap (Assign . App (Const asgnRout)) parseArg,
            symbol "rally" >> fmap (Assign . App (Const asgnRestore)) parseArg
        ]
    fmap (Seq cmd) nextExpr <|> return cmd
    where
    nextExpr = do
        skip (== ';')
        skipSpace
        exprParser
    symbol a = do
        s <- string a
        skipSpace
        return s
    parseArg = choice [
            fmap Const decimal,
            string "damage" >> return (Var dmg)
        ]

dmg :: Resolution -> Int
dmg (Res (_, Enemy d _ _ _)) = d

asgnKill :: Int -> Resolution -> Resolution
asgnKill k (Res (split, enemy)) = Res (removeLeft k split, enemy)

asgnRout :: Int -> Resolution -> Resolution
asgnRout k (Res (split, enemy)) = Res (moveRight k split, enemy)

asgnRestore :: Int -> Resolution -> Resolution
asgnRestore k (Res (split, enemy)) = Res (moveLeft k split, enemy)
