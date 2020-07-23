{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators #-}
module Language.Resolvers.Types (
    EType(..),
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
    EPair :: EType env a -> EType env b -> EType env (a, b)
    EList :: EType env a -> EType env [a]
    EFun :: EType env a -> EType env b -> EType env (a -> b)


instance TestEquality (EType env) where
    testEquality EBool EBool = Just Refl
    testEquality EInt EInt = Just Refl
    testEquality EFloat EFloat = Just Refl
    testEquality EUnit EUnit = Just Refl
    testEquality (EPair a b) (EPair c d) = do
        Refl <- testEquality a c
        Refl <- testEquality b d
        Just Refl
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
typeRepr p@(EPair _ _) = "(" <> typeList p <> ")"
typeRepr (EList t) = "List of " <> typeRepr t
typeRepr (EFun a r) = typeRepr a <> " -> " <> typeRepr r

instance Show (EType e a) where
    show = unpack . typeRepr

typeList :: EType env a -> Text
typeList (EPair l r) = typeRepr l <> ", " <> typeList r
typeList t = typeRepr t
