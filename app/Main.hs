module Main where

import AI.Neuron (Network, readNetwork, sigmoid)

import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Attoparsec.Text (double)
import Data.Legio (Legio, Card(..))
import qualified Data.Legio as Legio
import Data.List (replicate)

import System.Environment (getArgs)
import System.IO (IOMode(..), openFile, hClose)
import System.Random (StdGen, getStdGen)

import UI.Player
import UI.ConsolePlayer
import UI.AiPlayer


main :: IO ()
main = do
    putStrLn "Welcome to LEGIO v. 0.1"
    [count, deck1, deck2] <- getArgs
    netFile <- openFile "data/legio.ai" ReadMode
    Right neural <- runEitherT $ readNetwork netFile sigmoid double
    hClose netFile
    rand <- getStdGen
    (legio1, legio2) <- flip evalRandT rand $ do
        l1 <- mkLegio (read count) (read deck1)
        l2 <- mkLegio (read count) (read deck2)
        return (l1, l2)
    gameLoop neural legio1 legio2


mkLegio :: Int -> (Int, Int, Int) -> RandT StdGen IO Legio
mkLegio cohorts (a, d, r) = Legio.new 5 cohorts cards
    where
    cards = (replicate a Attack) ++ (replicate d Defend) ++ (replicate r Rally)

gameLoop :: Network Double -> Legio -> Legio -> IO ()
gameLoop neural legio1 legio2 = do
    let player1 = ConsolePlayer "Player 1"
    let player2 = AiPlayer "Player 2" neural
    displayStatus player1 legio1
    displayStatus player2 legio2
    putStrLn ""
    choice1 <- getCard player1 (Legio.cohorts legio2) legio1
    choice2 <- getCard player2 (Legio.cohorts legio1) legio2
    let (legio1', legio2') = Legio.resolve choice1 choice2
    case (Legio.isRouted legio1', Legio.isRouted legio2') of
        (True, True) -> do
            putStrLn "Both armies have been routed!"
            putStrLn (
                    "Survivors: player 1 – " ++ show (Legio.routed legio1') ++
                    " cohorts, player 2 – " ++ show (Legio.routed legio2') ++
                    " cohorts."
                )

        (True, False) ->
            putStrLn (
                    "Player 2 wins with " ++ show (Legio.active legio2') ++
                    " fighting cohorts and " ++ show (Legio.routed legio2') ++
                    " routed."
                )

        (False, True) ->
            putStrLn (
                "Player 1 wins with " ++ show (Legio.active legio1') ++
                " fighting cohorts and " ++ show (Legio.routed legio1') ++
                " routed."
            )

        (False, False) -> gameLoop neural legio1' legio2'

displayStatus :: PlayerUI p => p -> Legio -> IO ()
displayStatus player legio = do
    putStrLn (
            name player ++ " has got " ++ show (Legio.active legio) ++
            " fighting cohorts and " ++ show (Legio.routed legio) ++
            " retrerating ones."
        )
