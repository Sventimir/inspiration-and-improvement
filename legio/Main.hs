module Main where

import Config.Resolver (Resolver, loadResolver, resolve)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.State.Compose (composeState)
import Control.Monad.Trans.Either (runEitherT)

import Data.Card (Card(..), CardCounts, count)
import Data.CardSet (CardSet, cardSetFromList, deck, discard, hand)
import Data.Split (Split, split, left)
import qualified Data.Text as Text

import System.Environment (getArgs)
import System.Random (StdGen, getStdGen)

import UI.ConsolePlayer
import UI.DummyPlayer
import UI.Player (PlayerUI(..), Enemy(..), Player(..), playCard)


handSize = 5

main :: IO ()
main = do
    putStrLn "Welcome to LEGIO v. 0.2"
    [playerCards, enemyCards] <- getArgs
    resolverOrError <- runEitherT $ loadResolver "data/resolver.conf"
    resolver <- case resolverOrError of
        Right r -> return r
        Left e -> error $ Text.unpack e
    rand <- getStdGen
    (player, enemy) <- flip evalRandT rand $ do
        pl <- mkPlayer (ConsolePlayer "Player") $ read playerCards
        en <- mkPlayer (DummyPlayer "Enemy") $ read enemyCards
        return (pl, en)
    evalStateT (gameLoop resolver) (player, enemy)

gameLoop :: Resolver -> StateT (Player, Player) IO ()
gameLoop resolver = do
    (player, enemy) <- get
    (playerCard, enemyCard) <- composeState
        (playCard $ enemyFromPlayer enemy)
        (playCard $ enemyFromPlayer player)
    (composeState
            (report enemyCard :: StateT Player IO ())
            (report playerCard :: StateT Player IO ())
            :: StateT (Player, Player) IO ()
        )
    resolve resolver playerCard enemyCard
    (player', enemy') <- get
    case (left $ legio player', left $ legio enemy') of
        (0, 0) -> liftIO $ putStrLn "Draw: both players routed!"
        (0, c) -> liftIO $ putStrLn ("Enemy wins with " ++ show c ++ " fighting cohorts!")
        (c, 0) -> liftIO $ putStrLn ("Player wins with " ++ show c ++ " fighting cohorts!")
        _ -> gameLoop resolver

mkPlayer :: PlayerUI p => (Int -> Split Int -> CardSet Card -> p) -> CardCounts -> RandT StdGen IO Player
mkPlayer constr (a, d, r) = do
    cardset <- cardSetFromList d (
            replicate a Attack ++
            replicate d Defend ++
            replicate r Rally
        )
    return . Player $ constr a (split (2 * r) d) cardset

enemyFromPlayer :: Player -> Enemy
enemyFromPlayer (Player p) =
    let cset = cardSet p
        discardCount = count $ discard cset
        totalCount = count (discard cset ++ hand cset ++ deck cset)
    in
    Enemy (damage p) (legio p) totalCount discardCount
