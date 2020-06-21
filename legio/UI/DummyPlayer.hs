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


data DummyPlayer = DummyPlayer String (Split Int) (CardSet Card)

instance Replaceable DummyPlayer (CardSet Card) where
    extract (DummyPlayer _ _ cs) = cs
    replace cs (DummyPlayer n s _) = DummyPlayer n s cs

instance Replaceable DummyPlayer (Split Int) where
    extract (DummyPlayer _ l _) = l
    replace l (DummyPlayer n _ cs) = DummyPlayer n l cs

instance PlayerUI DummyPlayer where
    name (DummyPlayer n _ _) = n
    legio (DummyPlayer _ l _) = l
    cardSet (DummyPlayer _ _ cs) = cs

    selectCard enemy (DummyPlayer n l cset) =
        case choose (0 :: Int) cset of
            Just c -> return c
            Nothing -> liftIO . throwIO $ AssertionFailed "Empty hand!"

    report _ = return ()
