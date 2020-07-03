{-# LANGUAGE GADTs #-}
module Language.Resolvers.Unchecked (
    UExpr(..)
) where

import Language.Resolvers.Types (EType)


data UExpr env where
    UConst :: EType env a -> a -> UExpr env
    UVar :: EType env a -> (env -> a) -> UExpr env
    UAssign :: UExpr env -> UExpr env
    UApp :: UExpr env -> UExpr env -> UExpr env
    USeq :: UExpr env -> UExpr env -> UExpr env
    UIf :: UExpr env -> UExpr env -> UExpr env -> UExpr env
    UWhile :: UExpr env -> UExpr env -> UExpr env

instance Show (UExpr e) where
    show (UConst t _) = "CONST(" <> show t <> ")"
    show (UVar t _) = "VAR(" <> show t <> ")"
    show (UAssign f) = "ASGN(" <> show f <> ")"
    show (UApp f a) = "(" <> show f <> " <- " <> show a <> ")"
    show (USeq a b) = show a <> "; " <> show b
    show (UIf b y n) = "IF (" <> show b <> ") THEN (" <> show y <> ") ELSE (" <> show n <> ")"
    show (UWhile b a) = "WHILE (" <> show b <> ") DO (" <> show a <> ")"
