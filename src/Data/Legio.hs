module Data.Legio (
    Card(..),
    cardFromSymbol,
    Legio,
    CardCount,
    new,
    shuffle,
    draw,
    playAndDraw,
    cohorts,
    active,
    routed,
    deck,
    discard,
    hand,
    isDead,
    isRouted,
    resolve,
    countCards
) where

import Control.Monad (mzero)
import Control.Monad.Random (RandomGen, RandT)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.List (delete, elem, foldl')
import Data.Split
import System.Random.Shuffle (shuffleM)


data Card = Attack | Defend | Rally deriving (Eq, Show)

cardFromSymbol :: Char -> Maybe Card
cardFromSymbol 'A' = Just Attack
cardFromSymbol 'a' = Just Attack
cardFromSymbol 'D' = Just Defend
cardFromSymbol 'd' = Just Defend
cardFromSymbol 'R' = Just Rally
cardFromSymbol 'r' = Just Rally
cardFromSymbol _ = Nothing


data Legio = Legio {
    cohorts :: Split Int, -- left are active, right are routed
    deck, discard, hand :: [Card]
} deriving (Eq, Show)

type CardCount = (Int, Int, Int)

new :: (RandomGen r, Monad m) => Int -> Int -> [Card] -> RandT r m Legio
new handSize cohorts cards = do
    d <- shuffleM cards
    return $ Legio {
        cohorts = split cohorts 0,
        deck = drop handSize d,
        discard = [],
        hand = take handSize d
    }

active :: Legio -> Int
active (Legio { cohorts = c }) = left c

routed :: Legio -> Int
routed (Legio { cohorts = c }) = right c

shuffle :: (RandomGen r, Monad m) => Legio -> RandT r m Legio
shuffle legio = do
    newDeck <- shuffleM $ discard legio
    return $ legio { discard = [], deck = newDeck }

draw :: (RandomGen r, MonadFail m) => Legio -> RandT r m Legio
draw l@(Legio { deck = [], discard = [] }) = return l
draw l@(Legio { deck = [], discard = d, hand = h}) = do
    (c : cs) <- shuffleM d
    return $ l { discard = [], hand = c : h, deck = cs}
draw l@(Legio { deck = (c : cs), hand = h }) =
    return $ l { deck = cs, hand = c : h }

playAndDraw :: (RandomGen r, Monad m) => Legio -> Card -> RandT r (MaybeT m) (Legio, Card)
playAndDraw legio@(Legio { hand = h, discard = d }) c
    | c `elem` h = do
        l <- draw $ legio { hand = delete c h, discard = c : d }
        return (l, c)
    | otherwise = mzero

isDead :: Legio -> Bool
isDead (Legio { cohorts = c }) = total c <= 0

isRouted :: Legio -> Bool
isRouted (Legio { cohorts = c }) = left c <= 0

rally :: Int -> Legio -> Legio
rally n legio@(Legio { cohorts = c }) = legio { cohorts = moveLeft n c }

rout :: Int -> Legio -> Legio
rout n legio@(Legio { cohorts = c }) = legio { cohorts = moveRight n c }

kill :: Int -> Legio -> Legio
kill n legio@(Legio { cohorts = c }) = legio { cohorts = removeLeft n c }

resolve :: (Legio, Card) -> (Legio, Card) -> (Legio, Legio)
resolve (legio1, Attack) (legio2, Attack) = (
        kill 1 $ rout 2 legio1,
        kill 1 $ rout 2 legio2
    )
resolve (legio1, Attack) (legio2, Defend) = (
        rout 2 legio1,
        rout 1 legio2
    )
resolve (legio1, Attack) (legio2, Rally) = (
        rout 2 legio1,
        kill 2 legio2
    )
resolve (legio1, Defend) (legio2, Attack) = (
        rout 1 legio1,
        rout 2 legio2
    )
resolve (legio1, Defend) (legio2, Defend) = (
        rout 1 legio1,
        rout 1 legio2
    )
resolve (legio1, Defend) (legio2, Rally) = (
        rout 1 legio1,
        rally 2 legio2
    )
resolve (legio1, Rally) (legio2, Attack) = (
        kill 2 legio1,
        rout 1 legio2
    )
resolve (legio1, Rally) (legio2, Defend) = (
        rally 2 legio1,
        rout 1 legio2
    )
resolve (legio1, Rally) (legio2, Rally) = (
        rally 2 legio1,
        rally 2 legio2
    )

countCards :: [Card] -> CardCount
countCards = foldl' classify (0, 0, 0)
    where
    classify (a, d, r) Attack = (succ a, d, r)
    classify (a, d, r) Defend = (a, succ d, r)
    classify (a, d, r) Rally  = (a, d, succ r)
