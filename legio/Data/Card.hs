{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeSynonymInstances #-}
module Data.Card where

import Data.Attoparsec.Text (Parser, choice, string)
import Data.CardSet (Selector(..))
import Data.List (find, foldl')


data Card = Attack | Defend | Rally
    deriving (Show, Eq, Ord)

instance Selector String Card where
    select "a" = find (== Attack)
    select "A" = find (== Attack)
    select "d" = find (== Defend)
    select "D" = find (== Defend)
    select "r" = find (== Rally)
    select "R" = find (== Rally)
    select _ = const Nothing

type CardCounts = (Int, Int, Int)

count :: [Card] -> CardCounts
count = foldl' incr (0, 0, 0)
    where
    incr (a, d, r) Attack = (succ a, d, r)
    incr (a, d, r) Defend = (a, succ d, r)
    incr (a, d, r) Rally  = (a, d, succ r)

cardParser :: Parser Card
cardParser = choice [
        string "Attack" >> return Attack,
        string "Defend" >> return Defend,
        string "Rally"  >> return Rally
    ]
