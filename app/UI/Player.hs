module UI.Player (
    Enemy(..),
    ValidChoice, -- opaque, as type system cannot ensure the choice is indeed valid
    Validator,
    PlayerUI(..),
    enemyFromLegio,
    getCard
) where

import Control.Monad.Random (evalRandT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Legio (Card, CardCount, Legio)
import qualified Data.Legio as Legio
import Data.Split (Split)

import System.Random (StdGen, getStdGen)


data Enemy = Enemy (Split Int) CardCount CardCount

enemyFromLegio :: Legio -> Enemy
enemyFromLegio l =
    Enemy (Legio.cohorts l) (Legio.countCards $ Legio.discard l) (Legio.countCards allCards)
    where
    allCards = Legio.hand l ++ Legio.deck l

newtype ValidChoice = ValidChoice (Legio, Card)
type Validator = Card -> MaybeT IO ValidChoice

class PlayerUI ui where
    name :: ui -> String
    legio :: ui -> Legio
    update :: ui -> Legio -> ui
    selectCard :: ui -> Enemy -> Validator -> IO ValidChoice

    train :: ui -> Card -> Enemy -> ui
    train ui _ _ = ui


getCard :: PlayerUI ui => ui -> Enemy -> IO (Legio, Card)
getCard ui enemy = do
    ValidChoice c <- selectCard ui enemy validate
    return c
    where
    validate card = do
        rand <- lift getStdGen
        evalRandT (fmap ValidChoice $ Legio.playAndDraw (legio ui) card) rand
