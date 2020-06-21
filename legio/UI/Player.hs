{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables #-}
module UI.Player (
    Enemy(..),
    Player(..),
    PlayerUI(..),
    ValidChoice, -- opaque, because type system cannot guarantee validity
    playCard
) where

import Control.Monad (join, mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (StateT(..), execStateT, get, modify, put)
import Control.Monad.State.Compose (Replaceable(..), Splitable(..), composeState, partialState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Card (Card(..), CardCounts, count)
import Data.CardSet (CardSet, Selector(..), ValidChoice, draw, play)
import Data.Split (Split, moveLeft, moveRight, removeLeft)
import Data.Tuple (swap)
import Data.Tuple.Extra (both)

import System.Random (StdGen, getStdGen)


data Enemy = Enemy Int (Split Int) CardCounts CardCounts

class (Replaceable p (CardSet Card), Replaceable p (Split Int)) => PlayerUI p where
    name :: p -> String
    damage :: p -> Int
    legio :: p -> Split Int
    cardSet :: p -> CardSet Card
    selectCard :: MonadIO m => Enemy -> p -> m (ValidChoice Card)
    report :: MonadIO m => Card -> StateT p m ()

data Player where
    Player :: PlayerUI p => p -> Player

instance Replaceable Player (Split Int) where
    extract (Player p) = extract p
    replace s (Player p) = Player $ replace s p

instance Replaceable Player (CardSet Card) where
    extract (Player p) = extract p
    replace cset (Player p) = Player $ replace cset p

instance PlayerUI Player where
    name (Player p) = name p
    damage (Player p) = damage p
    legio (Player p) = legio p
    cardSet (Player p) = cardSet p
    selectCard enemy (Player p) = selectCard enemy p
    report card = StateT $ \(Player p) -> do
        p' <- execStateT (report card) p
        return ((), Player p')

playCard :: forall m . MonadIO m => Enemy -> StateT Player m Card
playCard enemy = do
    choice <- get >>= selectCard enemy
    card <- partialState $ play choice
    rand <- liftIO getStdGen
    partialState $ evalRandT (draw 1 :: RandT StdGen (StateT (CardSet Card) m) ()) rand
    return card
