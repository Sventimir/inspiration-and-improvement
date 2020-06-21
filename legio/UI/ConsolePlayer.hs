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


data ConsolePlayer = ConsolePlayer String Int (Split Int) (CardSet Card)

instance Replaceable ConsolePlayer (CardSet Card) where
    extract (ConsolePlayer _ _ _ cs) = cs
    replace cs (ConsolePlayer n a s _) = ConsolePlayer n a s cs

instance Replaceable ConsolePlayer (Split Int) where
    extract (ConsolePlayer _ _ l _) = l
    replace l (ConsolePlayer n a _ cs) = ConsolePlayer n a l cs

instance PlayerUI ConsolePlayer where
    name (ConsolePlayer n _ _ _) = n
    damage (ConsolePlayer _ d _ _) = d
    legio (ConsolePlayer _ _ l _) = l
    cardSet (ConsolePlayer _ _ _ cs) = cs

    selectCard (Enemy _ enemy _ _) (ConsolePlayer n _ l cset) = liftIO $ do
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
