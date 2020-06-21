{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Monad.State.Compose (
    Replaceable(..),
    Splitable(..),
    Wrapper(..),
    chainState,
    composeState,
    forEachState,
    partialState,
    readOnly,
    wrapped
) where

import Control.Monad (mapM)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT(..), runStateT, get, gets)

import Data.List (foldl')


class Wrapper w a where
    wrap :: a -> w
    unwrap :: w -> a

class Splitable s a b where
    split :: s -> (a, b)
    fuse :: a -> b -> s

class Replaceable s p where
    extract :: s -> p
    replace :: p -> s -> s

instance Splitable (a, b) a b where
    split = id
    fuse a b = (a, b)

instance Splitable a a () where
    split a = (a, ())
    fuse a () = a

wrapped :: (Wrapper w s, Monad m) => StateT s m a -> StateT w m a
wrapped action = StateT $ \w -> do
    (b, s) <- runStateT action $ unwrap w
    return (b, wrap s)

composeState :: (Monad m, Splitable c a b, Splitable t s r) =>
    StateT s m a -> StateT r m b -> StateT t m c
composeState left right = StateT $ \t -> do
    let (s, r) = split t
    (a, s') <- runStateT left s
    (b, r') <- runStateT right r
    return (fuse a b, fuse s' r')


forEachState :: Monad m => StateT s m a -> StateT [s] m [a]
forEachState action = StateT $ \states ->
    fmap unzip $ mapM (runStateT action) states

foldlIntoState :: Monad m => (a -> StateT s m s) -> [a] -> StateT s m s
foldlIntoState action inputs = StateT (foldInputs inputs)
    where
    foldInputs [] s = return (s, s)
    foldInputs (i : is) s = do
        (s', _) <- runStateT (action i) s
        foldInputs is s'

chainState :: Monad m => (a -> StateT s m a) -> a -> StateT [s] m a
chainState action init = StateT $ foldInputs init
    where
    foldInputs acc [] = return (acc, [])
    foldInputs acc (s : ss) = do
        (a, s') <- runStateT (action acc) s
        (a', ss') <- foldInputs a ss
        return (a', s' : ss')

partialState :: (Monad m, Replaceable s p) => StateT p m a -> StateT s m a
partialState action = StateT $ \s -> do
    (a, p) <- runStateT action $ extract s
    return $ (a, replace p s)

readOnly :: Monad m => ReaderT s m a -> StateT s m a
readOnly r = StateT $ \s -> do
    a <- runReaderT r s
    return (a, s)
