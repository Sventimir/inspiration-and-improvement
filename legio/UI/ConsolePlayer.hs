{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module UI.ConsolePlayer (
    ConsolePlayer(..) -- selects always the first card.
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Compose (Replaceable(..))

import Data.Card (Card(..))
import Data.CardSet (CardSet(..), choose)
import Data.Split (Split, left, right)

import UI.Player


data ConsolePlayer = ConsolePlayer String (Split Int) (CardSet Card)

instance Replaceable ConsolePlayer (CardSet Card) where
    extract (ConsolePlayer _ _ cs) = cs
    replace cs (ConsolePlayer n s _) = ConsolePlayer n s cs

instance Replaceable ConsolePlayer (Split Int) where
    extract (ConsolePlayer _ l _) = l
    replace l (ConsolePlayer n _ cs) = ConsolePlayer n l cs

instance PlayerUI ConsolePlayer where
    name (ConsolePlayer n _ _) = n
    legio (ConsolePlayer _ l _) = l
    cardSet (ConsolePlayer _ _ cs) = cs

    selectCard (Enemy enemy _ _) (ConsolePlayer n l cset) = liftIO $ do
        putStrLn (
                n ++ ", you've got " ++ (show $ left l) ++ " fighting cohorts and " ++
                (show $ right l) ++ " retreating cohorts."
            )
        putStrLn (
                "Your enemy has " ++ (show $ left enemy) ++ " fighting cohorts and " ++
                (show $ right enemy) ++ " retreating cohorts."
            )
        putStrLn (
                "You've got " ++ (show . length $ deck cset) ++ " cards remaining in your deck and " ++
                (show . length $ discard cset) ++ " discarded."
            )
        putStrLn (
                "Your hand: " ++ (show $ hand cset)
            )
        putStrLn "Select the card to play: "
        propmptSelect
        where
        propmptSelect = do
            choice <- getLine
            case choose choice cset of
                Nothing -> do
                    putStrLn "Invalid selection."
                    propmptSelect
                Just c -> return c

    report card = liftIO $ putStrLn ("Enemy played: " ++ show card ++ ".")
