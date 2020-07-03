{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Resolvers.Compiler (
    compile,
    infer,
    typeCheck
) where

import Data.Text (Text)
import Data.Type.Equality ((:~:)(..))

import Language.Resolvers.Expr (Expr(..))
import Language.Resolvers.Types (EType(..), assertType, typeRepr)
import Language.Resolvers.Unchecked (UExpr(..))


data Term env where
    Term :: EType a -> Expr env a -> Term env


compile :: UExpr env -> Either Text (Expr env ())
compile = typeCheck EUnit

typeCheck :: EType a -> UExpr env -> Either Text (Expr env a)
typeCheck expected uexpr = do
    Term t e <- infer uexpr
    Refl <- assertType expected t
    return e

infer :: UExpr env -> Either Text (Term env)
infer (UConst t a) = return $ Term t (Const a)
infer (UVar t getter) = return $ Term t (Var getter)
infer (UAssign t asgn v) = do
    Term tv a <- infer v
    Refl <- assertType t tv
    return $ Term EUnit (Assign asgn a)
infer (UApp uf ua) = do
    Term tf f <- infer uf
    case tf of
        EFun tArg tRet -> do
            Term ta a <- infer ua
            Refl <- assertType tArg ta
            return $ Term tRet (App f a)
        _ -> Left (typeRepr tf <> " is not a function type.")
infer (USeq ua ub) = do
    Term ta a <- infer ua
    Refl <- assertType EUnit ta
    Term tb b <- infer ub
    case a of
        Const () -> return $ Term tb b
        _ -> return $ Term tb (Seq a b)
infer (UIf cond ifSo ifNot) = do
    Term tbool c <- infer cond
    Refl <- assertType EBool tbool
    Term tRet y <- infer ifSo
    Term tRet' n <- infer ifNot
    Refl <- assertType tRet tRet'
    return $ Term tRet (IfThenElse c y n)
infer (UWhile cond ua) = do
    Term tbool c <- infer cond
    Refl <- assertType EBool tbool
    Term tunit a <- infer ua
    Refl <- assertType EUnit tunit
    case a of
        Const () -> return $ Term EUnit (Const ())
        _ -> return $ Term EUnit (While c a)
