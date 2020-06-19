module Data.Card where

import Data.List (foldl')


data Card = Attack | Defend | Rally
type CardCounts = (Int, Int, Int)

count :: [Card] -> CardCounts
count = foldl' incr (0, 0, 0)
    where
    incr (a, d, r) Attack = (succ a, d, r)
    incr (a, d, r) Defend = (a, succ d, r)
    incr (a, d, r) Rally  = (a, d, succ r)
