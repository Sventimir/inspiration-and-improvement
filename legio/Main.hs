module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.State.Compose (composeState)

import Data.Card (Card(..), CardCounts, count)
import Data.CardSet (CardSet, cardSetFromList, deck, discard, hand)
import Data.Split (Split, split, left)

import System.Environment (getArgs)
import System.Random (StdGen, getStdGen)

import UI.ConsolePlayer
import UI.DummyPlayer
import UI.Player


handSize = 5

main :: IO ()
main = do
    putStrLn "Welcome to LEGIO v. 0.2"
    [playerCards, enemyCards] <- getArgs
    rand <- getStdGen
    (player, enemy) <- flip evalRandT rand $ do
        pl <- mkPlayer (ConsolePlayer "Player") $ read playerCards
        en <- mkPlayer (DummyPlayer "Enemy") $ read enemyCards
        return (pl, en)
    evalStateT gameLoop (player, enemy)

gameLoop :: StateT (Player, Player) IO ()
gameLoop = do
    (player, enemy) <- get
    (playerCard, enemyCard) <- composeState
        (playCard $ enemyFromPlayer enemy)
        (playCard $ enemyFromPlayer player)
    (composeState
            (report enemyCard :: StateT Player IO ())
            (report playerCard :: StateT Player IO ())
            :: StateT (Player, Player) IO ()
        )
    resolve playerCard enemyCard
    (player', enemy') <- get
    case (left $ legio player', left $ legio enemy') of
        (0, 0) -> liftIO $ putStrLn "Draw: both players routed!"
        (0, c) -> liftIO $ putStrLn ("Enemy wins with " ++ show c ++ " fighting cohorts!")
        (c, 0) -> liftIO $ putStrLn ("Player wins with " ++ show c ++ " fighting cohorts!")
        _ -> gameLoop

mkPlayer :: PlayerUI p => (Int -> Split Int -> CardSet Card -> p) -> CardCounts -> RandT StdGen IO Player
mkPlayer constr (a, d, r) = do
    cardset <- cardSetFromList d (
            replicate a Attack ++
            replicate d Defend ++
            replicate r Rally
        )
    return . Player $ constr a (split r d) cardset

enemyFromPlayer :: Player -> Enemy
enemyFromPlayer (Player p) =
    let cset = cardSet p
        discardCount = count $ discard cset
        totalCount = count (discard cset ++ hand cset ++ deck cset)
    in
    Enemy (legio p) totalCount discardCount
