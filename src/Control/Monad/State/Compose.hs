{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.State.Compose (
    Wrapper(..),
    Splitable(..),
    chainState,
    compose,
    forEachState,
    wrapped
) where

import Control.Monad (mapM)
import Control.Monad.State (StateT(..), runStateT)

import Data.List (foldl')

class Wrapper w a where
    wrap :: a -> w
    unwrap :: w -> a

class Splitable s a b where
    split :: s -> (a, b)
    fuse :: a -> b -> s

wrapped :: (Wrapper w s, Monad m) => StateT s m a -> StateT w m a
wrapped action = StateT $ \w -> do
    (b, s) <- runStateT action $ unwrap w
    return (b, wrap s)

compose :: (Monad m, Splitable c a b, Splitable t s r) =>
    StateT s m b -> StateT r m a -> StateT t m c
compose left right = StateT $ \t -> do
    let (s, r) = split t
    (b, s') <- runStateT left s
    (a, r') <- runStateT right r
    return (fuse a b, fuse s r)


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
