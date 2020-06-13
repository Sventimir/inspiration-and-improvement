module UI.AiPlayer (
    AiPlayer(..)
) where

import AI.Neuron (Network, eval)

import Control.Monad.Trans.Maybe (runMaybeT)

import Data.Legio (Card(..))
import qualified Data.Legio as Legio
import Data.List (foldl', maximumBy)
import Data.Split (total, left, right)

import UI.Player


data AiPlayer = AiPlayer String (Network Double)


instance PlayerUI AiPlayer where
    name (AiPlayer n _) = n

    selectCard (AiPlayer name net) enemy legio validate =
        let enemyCohorts = fromIntegral $ total enemy
            ownCohorts = fromIntegral . total $ Legio.cohorts legio
            (handA, handD, handR) = foldl' countCards (0, 0, 0) $ Legio.hand legio
            (deckA, deckD, deckR) = foldl' countCards (0, 0, 0) $ Legio.deck legio
            handCards = handA + handD + handR
            deckCards = deckA + deckD + deckR
            inputs = [
                    ownCohorts / (enemyCohorts + ownCohorts),
                    (fightingCohorts $ Legio.cohorts legio) / ownCohorts,
                    fightingCohorts enemy / enemyCohorts,

                    fromIntegral handA / fromIntegral handCards,
                    fromIntegral handD / fromIntegral handCards,
                    fromIntegral handR / fromIntegral handCards,

                    fromIntegral deckA / fromIntegral deckCards,
                    fromIntegral deckD / fromIntegral deckCards,
                    fromIntegral deckR / fromIntegral deckCards
                ]
            (card, _) = maximumBy cmpWeights . zip [Attack, Defend, Rally] $ eval net inputs
        in do
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
        fallback = do
            Just decision <- runMaybeT . validate . head $ Legio.hand legio
            return decision
