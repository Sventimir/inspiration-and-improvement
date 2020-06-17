module UI.DummyPlayer (
    DummyPlayer(..)
) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Legio (Legio(..))
import UI.Player

data DummyPlayer = DummyPlayer String Legio


instance PlayerUI DummyPlayer where
    name (DummyPlayer n _) = n
    legio (DummyPlayer _ l) = l
    update (DummyPlayer n _) l = DummyPlayer n l

    selectCard (DummyPlayer n legio) _ validate = do
        Just result <- runMaybeT . validate . head $ hand legio
        return result
