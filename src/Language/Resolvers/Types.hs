{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators #-}
module Language.Resolvers.Types (
    EType(..),
    assertType,
    typeRepr
) where

import Data.Text (Text, unpack)
import Data.Type.Equality (TestEquality(..), (:~:)(..))


-- Internal types of the language.
data EType env a where
    EUnit :: EType env ()
    EBool :: EType env Bool
    EInt :: EType env Int
    EFloat :: EType env Double
    EAlter :: EType env (env -> env)
    EList :: EType env a -> EType env [a]
    EFun :: EType env a -> EType env b -> EType env (a -> b)


instance TestEquality (EType env) where
    testEquality EBool EBool = Just Refl
    testEquality EInt EInt = Just Refl
    testEquality EFloat EFloat = Just Refl
    testEquality EUnit EUnit = Just Refl
    testEquality EAlter EAlter = Just Refl
    testEquality (EList a) (EList b) = do
        Refl <- testEquality a b
        Just Refl
    testEquality (EFun a r) (EFun b s) = do
        Refl <- testEquality a b
        Refl <- testEquality r s
        Just Refl
    testEquality _ _ = Nothing

typeRepr :: EType env a -> Text
typeRepr EBool = "Bool"
typeRepr EInt = "Int"
typeRepr EFloat = "Float"
typeRepr EUnit = "Unit"
typeRepr EAlter = "Alternator"
typeRepr (EList t) = "List of " <> typeRepr t
typeRepr (EFun a r) = typeRepr a <> " -> " <> typeRepr r

instance Show (EType e a) where
    show = unpack . typeRepr

assertType :: EType env a -> EType env b -> Either Text (a :~: b)
assertType expected actual = case testEquality expected actual of
    Just Refl -> return Refl
    Nothing -> Left $ typeMismatch expected actual

typeMismatch :: EType env a -> EType env b -> Text
typeMismatch expected actual = "This expression should have type " <>
    typeRepr expected <> " but it has type " <> typeRepr actual <> " instead."
