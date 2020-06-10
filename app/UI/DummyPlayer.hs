module UI.DummyPlayer (
    DummyPlayer(..)
) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Legio (Legio(..))
import UI.Player

newtype DummyPlayer = DummyPlayer String


instance PlayerUI DummyPlayer where
    name (DummyPlayer n) = n

    selectCard (DummyPlayer n) legio validate = do
        Just result <- runMaybeT . validate . head $ hand legio
        return result
