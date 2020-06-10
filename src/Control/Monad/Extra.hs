module Control.Monad.Extra (
    liftMaybe
) where

import Control.Monad.Trans.Maybe (MaybeT(..))


liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
