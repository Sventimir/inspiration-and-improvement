module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.State.Compose (composeState)

import Data.Card (Card(..), count)
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
    [cohorts, playerCards, enemyCards] <- getArgs
    rand <- getStdGen
    (playerDeck, enemyDeck) <- flip evalRandT rand $ do
        pl <- mkDeck $ read playerCards
        en <- mkDeck $ read enemyCards
        return (pl, en)
    let player = Player $ ConsolePlayer "Player" (mkLegio $ read cohorts) playerDeck
        enemy = Player $ DummyPlayer "Enemy" (mkLegio $ read cohorts) enemyDeck
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


mkLegio :: Int -> Split Int
mkLegio cs = split cs 0

mkDeck :: (Int, Int, Int) -> RandT StdGen IO (CardSet Card)
mkDeck (a, d, r) = cardSetFromList handSize (
        replicate a Attack ++
        replicate d Defend ++
        replicate r Rally
    )

enemyFromPlayer :: Player -> Enemy
enemyFromPlayer (Player p) =
    let cset = cardSet p
        discardCount = count $ discard cset
        totalCount = count (discard cset ++ hand cset ++ deck cset)
    in
    Enemy (legio p) totalCount discardCount
