{-# LANGUAGE GADTs #-}
module Main where

import AI.Neuron (Network(..), readNetwork)
import AI.Neuron.Perceptron (SigmoidPerceptron)
import AI.SimpleNetwork (SimpleNetwork)

import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Attoparsec.Text (double)
import Data.Legio (Legio, Card(..))
import qualified Data.Legio as Legio
import Data.List (replicate)

import System.Environment (getArgs)
import System.IO (FilePath, IOMode(..), openFile, hClose)
import System.Random (StdGen, getStdGen)

import UI.Player
import UI.ConsolePlayer
import UI.AiPlayer


data Player where
    Player :: PlayerUI p => p -> Player

main :: IO ()
main = do
    putStrLn "Welcome to LEGIO v. 0.2"
    [count, deck1, deck2] <- getArgs
    neural <- loadNeuralNetwork "data/legio.ai"
    rand <- getStdGen
    (legio1, legio2) <- flip evalRandT rand $ do
        l1 <- mkLegio (read count) (read deck1)
        l2 <- mkLegio (read count) (read deck2)
        return (l1, l2)
    let player1 = Player $ ConsolePlayer "Player" legio1
    let player2 = Player $ AiPlayer "Enenmy" legio2 neural
    gameLoop player1 player2


mkLegio :: Int -> (Int, Int, Int) -> RandT StdGen IO Legio
mkLegio cohorts (a, d, r) = Legio.new 5 cohorts cards
    where
    cards = (replicate a Attack) ++ (replicate d Defend) ++ (replicate r Rally)

loadNeuralNetwork :: FilePath -> IO (SimpleNetwork SigmoidPerceptron Double)
loadNeuralNetwork filename = do
    netFile <- openFile filename ReadMode
    result <- runEitherT $ readNetwork netFile double
    hClose netFile
    case result of
        Right neural -> return neural
        Left e -> do
            putStrLn "Could not load AI!"
            error e

gameLoop :: Player -> Player -> IO ()
gameLoop (Player player1) (Player player2) = do
    displayStatus player1
    displayStatus player2
    putStrLn ""
    let legio1 = legio player1
    let legio2 = legio player2
    choice1 <- getCard player1 (enemyFromLegio legio2)
    choice2@(_, card) <- getCard player2 (enemyFromLegio legio1)
    let (legio1', legio2') = Legio.resolve choice1 choice2
    putStrLn ("Player 2 played: " ++ show card ++ ".\n")
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

        (False, False) -> gameLoop (Player $ update player1 legio1') (Player $ update player2 legio2')

displayStatus :: PlayerUI p => p -> IO ()
displayStatus player = do
    let l = legio player
    putStrLn (
            name player ++ " has got " ++ show (Legio.active l) ++
            " fighting cohorts and " ++ show (Legio.routed l) ++
            " retrerating ones."
        )
