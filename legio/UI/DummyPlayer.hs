{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module UI.DummyPlayer (
    DummyPlayer(..) -- selects always the first card.
) where

import Control.Exception.Base (AssertionFailed(..), throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Compose (Replaceable(..))

import Data.Card (Card(..))
import Data.CardSet (CardSet(..), choose)
import Data.List (uncons)
import Data.Split (Split, left, right)

import UI.Player


data DummyPlayer = DummyPlayer String Int (Split Int) (CardSet Card)

instance Replaceable DummyPlayer (CardSet Card) where
    extract (DummyPlayer _ _ _ cs) = cs
    replace cs (DummyPlayer n a s _) = DummyPlayer n a s cs

instance Replaceable DummyPlayer (Split Int) where
    extract (DummyPlayer _ _ l _) = l
    replace l (DummyPlayer n a _ cs) = DummyPlayer n a l cs

instance PlayerUI DummyPlayer where
    name (DummyPlayer n _ _ _) = n
    damage (DummyPlayer _ d _ _) = d
    legio (DummyPlayer _ _ l _) = l
    cardSet (DummyPlayer _ _ _ cs) = cs

    selectCard enemy (DummyPlayer _ _ _ cset) =
        case choose (0 :: Int) cset of
            Just c -> return c
            Nothing -> liftIO . throwIO $ AssertionFailed "Empty hand!"

    report _ = return ()
