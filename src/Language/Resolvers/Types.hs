{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators #-}
module Language.Resolvers.Types (
    EType(..),
    assertType,
    typeRepr
) where

import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))


-- Internal types of the language.
data EType a where
    EUnit :: EType ()
    EBool :: EType Bool
    EInt :: EType Int
    EFloat :: EType Double
    EList :: EType a -> EType [a]
    EFun :: EType a -> EType b -> EType (a -> b)


instance TestEquality EType where
    testEquality EBool EBool = Just Refl
    testEquality EInt EInt = Just Refl
    testEquality EFloat EFloat = Just Refl
    testEquality EUnit EUnit = Just Refl
    testEquality (EList a) (EList b) = do
        Refl <- testEquality a b
        Just Refl
    testEquality (EFun a r) (EFun b s) = do
        Refl <- testEquality a b
        Refl <- testEquality r s
        Just Refl
    testEquality _ _ = Nothing

typeRepr :: EType a -> Text
typeRepr EBool = "Bool"
typeRepr EInt = "Int"
typeRepr EFloat = "Float"
typeRepr EUnit = "Unit"
typeRepr (EList t) = "List of " <> typeRepr t
typeRepr (EFun a r) = typeRepr a <> " -> " <> typeRepr r

assertType :: EType a -> EType b -> Either Text (a :~: b)
assertType expected actual = case testEquality expected actual of
    Just Refl -> return Refl
    Nothing -> Left $ typeMismatch expected actual

typeMismatch :: EType a -> EType b -> Text
typeMismatch expected actual = "This expression should have type " <>
    typeRepr expected <> "but it has type " <> typeRepr actual <> " instead."
