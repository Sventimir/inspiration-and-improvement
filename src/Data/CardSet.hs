{-# LANGUAGE MultiParamTypeClasses #-}
module Data.CardSet (
    Selector(..),
    CardSet,
    cardSetFromList,
    draw
) where

import Control.Monad.Extra (liftMaybe)
import Control.Monad.Random (RandomGen, RandT)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)

import System.Random.Shuffle (shuffleM)

class Selector s a where
    select :: s -> [a] -> Maybe a

data CardSet a = CardSet { deck, hand, discard :: [a] }

cardSetFromList :: (Monad m, RandomGen r) => Int -> [a] -> RandT r m (CardSet a)
cardSetFromList handSize cards = do
    cs <- shuffleM cards
    return CardSet {
            hand = take handSize cs,
            deck = drop handSize cs,
            discard = []
        }

draw :: (Monad m, RandomGen r) => Int -> RandT r (StateT (CardSet a) m) ()
draw drawCount = do
    cset <- lift get
    let drw = take drawCount $ deck cset
        len = length drw
    if len >= drawCount then do
        lift . put $ cset { hand = drw ++ hand cset, deck = drop len $ deck cset }
    else do
        newDeck <- shuffleM $ discard cset
        let len' = drawCount - len
            drw' = take len' newDeck
        lift . put $ cset {
                hand = drw ++ drw' ++ hand cset,
                deck = drop len' newDeck,
                discard = []
            }

play :: (Eq a, Selector s a, Monad m) => s -> MaybeT (StateT (CardSet a) m) a
play selection = do
    cset <- lift get
    card <- liftMaybe . select selection $ hand cset
    lift . put $ cset { hand = delete card $ hand cset }
    return card
