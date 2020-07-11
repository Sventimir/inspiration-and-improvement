{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators #-}
module Language.Resolvers.Compiler (
    compile,
    infer,
    prettyError,
    typeCheck
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality ((:~:)(..), TestEquality(..))

import Language.Resolvers.Expr (Expr(..))
import Language.Resolvers.Types (EType(..), typeRepr)
import Language.Resolvers.Unchecked (Loc, UExpr(..), location)


data CompileError where
    NotAFunction :: Loc -> CompileError
    TypeMismatch :: Loc -> EType env a -> EType env b -> CompileError

data Term env where
    Term :: EType env a -> Expr env a -> Term env


compile :: UExpr env -> Either CompileError (Expr env ())
compile = typeCheck EUnit

typeCheck :: EType env a -> UExpr env -> Either CompileError (Expr env a)
typeCheck expected uexpr = do
    Term t e <- infer uexpr
    Refl <- assertType (location uexpr) expected t
    return e

infer :: UExpr env -> Either CompileError (Term env)
infer (UConst _ _ t a) = return $ Term t (Const a)
infer (UVar _ _ t getter) = return $ Term t (Var getter)
infer (UAssign uf) = do
    Term t f <- infer uf
    Refl <- assertType (location uf) EAlter t
    return $ Term EUnit (Assign f)
infer (UApp loc uf ua) = do
    Term tf f <- infer uf
    case tf of
        EFun tArg tRet -> do
            Term ta a <- infer ua
            Refl <- assertType loc tArg ta
            return $ Term tRet (App f a)
        _ -> Left . NotAFunction $ location uf
infer (USeq ua ub) = do
    Term ta a <- infer ua
    Refl <- assertType (location ua) EUnit ta
    Term tb b <- infer ub
    case a of
        Const () -> return $ Term tb b
        _ -> return $ Term tb (Seq a b)
infer (UIf loc cond ifSo ifNot) = do
    Term tbool c <- infer cond
    Refl <- assertType loc EBool tbool
    Term tRet y <- infer ifSo
    Term tRet' n <- infer ifNot
    Refl <- assertType (location ifNot) tRet tRet'
    return $ Term tRet (IfThenElse c y n)
infer (UWhile loc cond ua) = do
    Term tbool c <- infer cond
    Refl <- assertType loc EBool tbool
    Term tunit a <- infer ua
    Refl <- assertType (location ua) EUnit tunit
    case a of
        Const () -> return $ Term EUnit (Const ())
        _ -> return $ Term EUnit (While c a)

assertType :: Loc -> EType env a -> EType env b -> Either CompileError (a :~: b)
assertType loc expected actual = case testEquality expected actual of
    Just Refl -> return Refl
    Nothing -> Left $ TypeMismatch loc expected actual

prettyError :: Text -> CompileError -> Text
prettyError stream (NotAFunction (begin, end) ) =
    let loc = Text.pack $ show begin in
    "This expression is not a function, it cannot be applied:\n\t" <>
    loc <> " | ... " <> (Text.drop begin $ Text.take end stream) <> " ..."
prettyError stream (TypeMismatch (begin, end) expected actual) =
    let loc = Text.pack $ show begin in
    "Type mismatch at character #" <> loc <>
    ".\nExpected: " <> typeRepr expected <>
    ".\nActual: " <> typeRepr actual <>
    ".\n\t" <> loc <> " | ... " <> (Text.drop begin $ Text.take end stream) <> " ..."
