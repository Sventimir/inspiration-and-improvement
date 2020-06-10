module Main where

import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Legio (Legio, Card(..))
import qualified Data.Legio as Legio
import Data.List (replicate, uncons)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random (StdGen, getStdGen)


main :: IO ()
main = do
    putStrLn "Welcome to LEGIO v. 0.1"
    [count, deck1, deck2] <- getArgs
    rand <- getStdGen
    (legio1, legio2) <- flip evalRandT rand $ do
        l1 <- mkLegio (read count) (read deck1)
        l2 <- mkLegio (read count) (read deck2)
        return (l1, l2)
    gameLoop legio1 legio2


mkLegio :: Int -> (Int, Int, Int) -> RandT StdGen IO Legio
mkLegio cohorts (a, d, r) = Legio.new 5 cohorts cards
    where
    cards = (replicate a Attack) ++ (replicate d Defend) ++ (replicate r Rally)

gameLoop :: Legio -> Legio -> IO ()
gameLoop legio1 legio2 = do
    displayStatus "Player 1" legio1
    displayStatus "Player 2" legio2
    putStrLn ""
    player1 <- selectCard "Player 1" legio1
    player2 <- selectCard "Player 2" legio2
    let (legio1', legio2') = Legio.resolve player1 player2
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

        (False, False) -> gameLoop legio1' legio2'

displayStatus :: String -> Legio -> IO ()
displayStatus name legio = do
    putStrLn (
            name ++ " has got " ++ show (Legio.active legio) ++ " fighting cohorts and " ++
            show (Legio.routed legio) ++ " retrerating ones."
        )

selectCard :: String -> Legio -> IO (Legio, Card)
selectCard name legio = do
    putStrLn (
            name ++ ", your deck contains " ++ show (length $ Legio.deck legio) ++
            " cards and you've already discarded " ++ show (length $ Legio.discard legio)
        )
    putStrLn ("Your hand is: " ++ show (Legio.hand legio))
    getStdGen >>= getCard
    where
    getCard :: StdGen -> IO (Legio, Card)
    getCard rand = do
        putStr "Which card do you wish to play? [A/D/R]: "
        cardSym <- getLine
        let card = liftMaybe $ uncons cardSym >>= (Legio.cardFromSymbol . fst)
        result <- runMaybeT $ evalRandT (lift card >>= Legio.playAndDraw legio) rand
        case result of
            Nothing -> do
                putStrLn "Incorrect input!\n"
                hFlush stdout
                getCard rand
            Just selection -> do
                putStrLn "\n"
                return selection


liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
