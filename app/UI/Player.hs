module UI.Player (
    ValidChoice, -- opaque, as type system cannot ensure the choice is indeed valid
    Validator,
    PlayerUI(..),
    getCard
) where

import Control.Monad.Random (evalRandT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Legio (Card, Legio)
import qualified Data.Legio as Legio

import System.Random (StdGen, getStdGen)


newtype ValidChoice = ValidChoice (Legio, Card)
type Validator = Card -> MaybeT IO ValidChoice

class PlayerUI ui where
    name :: ui -> String
    selectCard :: ui -> Legio -> Validator -> IO ValidChoice


getCard :: PlayerUI ui => ui -> Legio -> IO (Legio, Card)
getCard ui legio = do
    ValidChoice c <- selectCard ui legio validate
    return c
    where
    validate card = do
        rand <- lift getStdGen
        evalRandT (fmap ValidChoice $ Legio.playAndDraw legio card) rand
