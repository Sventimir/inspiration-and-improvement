{-# LANGUAGE GADTs #-}
module Language.Resolvers.Expr (
    Expr(..),
    eval
) where

import Control.Monad.State (MonadState, gets, modify)


-- Type-checked and compiled expression ready for evaluation.
data Expr env a where
    Const :: a -> Expr env a
    Var :: (env -> a) -> Expr env a
    App :: Expr env (a -> b) -> Expr env a -> Expr env b
    Assign :: Expr env (env -> env) -> Expr env ()
    Seq :: Expr env () -> Expr env b -> Expr env b
    IfThenElse :: Expr env Bool -> Expr env a -> Expr env a -> Expr env a
    While :: Expr env Bool -> Expr env () -> Expr env ()

eval :: MonadState env m => Expr env a -> m a
eval (Const a) = return a
eval (Var f) = gets f
eval (App f a) = eval f <*> eval a
eval (Assign f) = eval f >>= modify
eval (Seq a b) = eval a >> eval b
eval (IfThenElse cond ifSo ifNot) = do
    c <- eval cond
    if c then eval ifSo else eval ifNot
eval w@(While cond ifSo) = do
    c <- eval cond
    if c then (eval ifSo >> eval w) else return ()
