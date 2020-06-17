module UI.ConsolePlayer (
    ConsolePlayer(..)
) where

import Control.Monad.Extra (liftMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Legio (Legio)
import qualified Data.Legio as Legio
import Data.List (uncons)

import System.IO (hFlush, stdout)
import UI.Player


data ConsolePlayer = ConsolePlayer String Legio


instance PlayerUI ConsolePlayer where
    name (ConsolePlayer n _) = n
    legio (ConsolePlayer _ l) = l
    update (ConsolePlayer n _) l = ConsolePlayer n l

    selectCard (ConsolePlayer n legio) _ validator = do
        putStrLn (
                n ++ ", your deck contains " ++ show (length $ Legio.deck legio) ++
                " cards and you've already discarded " ++ show (length $ Legio.discard legio)
            )
        putStrLn ("Your hand is: " ++ show (Legio.hand legio))
        getCard
        where
        getCard :: IO ValidChoice
        getCard = do
            putStr "Which card do you wish to play? [A/D/R]: "
            cardSym <- getLine
            let card = liftMaybe $ uncons cardSym >>= (Legio.cardFromSymbol . fst)
            result <- runMaybeT $ card >>= validator
            case result of
                Nothing -> do
                    putStrLn "Incorrect input!\n"
                    hFlush stdout
                    getCard
                Just selection -> do
                    putStrLn "\n"
                    return selection
