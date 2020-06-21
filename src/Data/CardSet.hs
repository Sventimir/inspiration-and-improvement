{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.CardSet (
    Selector(..),
    CardSet,
    ValidChoice,
    cardSetFromList,
    choose,
    deck,
    discard,
    draw,
    hand,
    play
) where

import Control.Monad.Extra (liftMaybe)
import Control.Monad.Random (RandomGen, RandT)
import Control.Monad.State (StateT, get, modify, put)
import Control.Monad.Trans (lift)

import Data.List (delete, find)
import System.Random.Shuffle (shuffleM)


class Selector s a where
    select :: s -> [a] -> Maybe a

instance Selector Int a where
    select i as
        | i >= 0 && i < length as = Just (as !! i)
        | otherwise = Nothing


data CardSet a = CardSet { deck, hand, discard :: [a] }
newtype ValidChoice a = ValidChoice a -- a container for card that can be safely played

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

choose :: Selector s a => s -> CardSet a -> Maybe (ValidChoice a)
choose s (CardSet { hand = h }) = fmap ValidChoice $ select s h

-- Since ValidChoice is opaque outside this module, it guarantees that the choice
-- is indeed valid.
play :: (Eq a, Monad m) => ValidChoice a -> StateT (CardSet a) m a
play (ValidChoice card) = do
    modify $ \cset -> cset { hand = delete card $ hand cset }
    return card
