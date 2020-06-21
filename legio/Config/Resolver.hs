{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving #-}
module Config.Resolver (
    Resolver,
    loadResolver,
    resolve
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Compose (composeState, partialState)
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

import System.IO (FilePath, IOMode(..), openFile)

import UI.Player (Enemy(..), Player(..), cardSet, damage, legio)


data Expr a where
    Pass :: Expr ()
    Seq :: Expr () -> Expr () -> Expr ()
    Int :: Int -> Expr Int
    Damage :: Expr Int
    Kill :: Expr Int -> Expr ()
    Rout :: Expr Int -> Expr ()
    Restore :: Expr Int -> Expr ()

deriving instance Show a => Show (Expr a)

type Resolver = Map (Card, Card) (Expr ())

apply :: Monad m => Expr a -> ReaderT Enemy (StateT (Split Int) m) a
apply Pass = return ()
apply (Seq l r) = apply l >> apply r
apply (Int i) = return i
apply Damage = ask >>= \(Enemy dmg _ _ _) -> return dmg
apply (Kill e) = apply e >>= (modify . removeLeft)
apply (Rout e) = apply e >>= (modify . moveRight)
apply (Restore e) = apply e >>= (modify . moveLeft)

execute :: Monad m => Enemy -> Expr () -> StateT (Split Int) m ()
execute enemy cmd = runReaderT (apply cmd) enemy

resolve :: forall m . Monad m => Resolver -> Card -> Card -> StateT (Player, Player) m ()
resolve resolver playerCard enemyCard = do
    let plCmd = fromMaybe Pass $ Map.lookup (playerCard, enemyCard) resolver
        enCmd = fromMaybe Pass $ Map.lookup (enemyCard, playerCard) resolver
    (player, enemy) <- get
    composeState
        (partialState $ execute (enemyFromPlayer enemy) plCmd :: StateT Player m ())
        (partialState $ execute (enemyFromPlayer player) enCmd :: StateT Player m ())


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

bodyParser :: Parser (Expr ())
bodyParser = do
    skip (== '{')
    skipSpace
    exprs <- many1' exprParser
    skipSpace
    skip (== '}')
    skipSpace
    return $ seqExpr exprs

seqExpr :: [Expr ()] -> Expr ()
seqExpr [] = Pass
seqExpr [e] = e
seqExpr (e : es) = Seq e $ seqExpr es

exprParser :: Parser (Expr ())
exprParser = do
    cmd <- choice [
            symbol "pass" >> return Pass,
            symbol "kill" >> fmap Kill parseArg,
            symbol "rout" >> fmap Rout parseArg,
            symbol "rally" >> fmap Restore parseArg
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
            fmap Int decimal,
            string "damage" >> return Damage
        ]
