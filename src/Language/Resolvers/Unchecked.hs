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
