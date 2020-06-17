{-# LANGUAGE GADTs #-}
module UI.AiPlayer (
    AiPlayer(..)
) where

import AI.Neuron (Network, feed, outputs)

import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)

import Data.Legio (Card(..), Legio(..))
import qualified Data.Legio as Legio
import Data.List (foldl', maximumBy)
import Data.Split (total, left, right)

import UI.Player


data AiPlayer n a where
    AiPlayer :: Network n => String -> Legio -> n a -> AiPlayer n a


instance (Network n, Floating a, Ord a, Show a) => PlayerUI (AiPlayer n a) where
    name (AiPlayer n _ _) = n
    legio (AiPlayer _ l _) = l

    update (AiPlayer name _ neural) legio = AiPlayer name legio neural

    selectCard (AiPlayer name legio net) (Enemy eSplit eDiscarded eAll) validate =
        let enemyCohorts = fromIntegral $ total eSplit
            ownCohorts = fromIntegral . total $ Legio.cohorts legio
            (handA, handD, handR) = Legio.countCards $ Legio.hand legio
            (deckA, deckD, deckR) = Legio.countCards $ Legio.deck legio
            (eDisA, eDisD, eDisR) = eDiscarded
            (eAllA, eAllD, eAllR) = eAll
            handCards = handA + handD + handR
            deckCards = deckA + deckD + deckR
            eTotalInPlay = eDisA + eDisD + eDisR - eAllA - eAllD - eAllR
            inputs = [
                    ownCohorts / (enemyCohorts + ownCohorts),
                    (fightingCohorts $ Legio.cohorts legio) / ownCohorts,
                    fightingCohorts eSplit / enemyCohorts,

                    fromIntegral handA / fromIntegral handCards,
                    fromIntegral handD / fromIntegral handCards,
                    fromIntegral handR / fromIntegral handCards,

                    fromIntegral deckA / fromIntegral deckCards,
                    fromIntegral deckD / fromIntegral deckCards,
                    fromIntegral deckR / fromIntegral deckCards,

                    fromIntegral (eAllA - eDisA) / fromIntegral eTotalInPlay,
                    fromIntegral (eAllD - eDisD) / fromIntegral eTotalInPlay,
                    fromIntegral (eAllR - eDisR) / fromIntegral eTotalInPlay
                ]
        in do
        results <- evalStateT (feed inputs) net
        let (card, _) = maximumBy cmpWeights $
                zipWith3 flattenUnavailable [Attack, Defend, Rally] [handA, handD, handR] results
        r <- runMaybeT $ validate card
        case r of
            Just decision -> return decision
            Nothing -> putStrLn "AI is confused!" >> fallback

        where
        fightingCohorts = fromIntegral . left
        countCards (a, d, r) Attack = (succ a, d, r)
        countCards (a, d, r) Defend = (a, succ d, r)
        countCards (a, d, r) Rally = (a, d, succ r)
        cmpWeights (_, l) (_, r) = compare l r
        flattenUnavailable card handRatio result
            | handRatio == 0 = (card, 0)
            | otherwise = (card, result)
        fallback = do
            Just decision <- runMaybeT . validate . head $ Legio.hand legio
            return decision
