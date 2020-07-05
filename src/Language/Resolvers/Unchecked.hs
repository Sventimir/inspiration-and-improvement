{-# LANGUAGE GADTs #-}
module Language.Resolvers.Unchecked (
    UExpr(..),
    UExprConstr(..),
    appByPrec,
    mkUExpr,
    precedence
) where

import Data.Text (Text, unpack)
import Language.Resolvers.Types (EType)


data UExprConstr env where
    CConst :: EType env a -> a -> UExprConstr env
    COper :: EType env (a -> b -> c) -> (a -> b -> c) -> Int -> UExprConstr env
    CVar :: EType env a -> (env -> a) -> UExprConstr env

data UExpr env where
    UConst :: Text -> EType env a -> a -> UExpr env
    UOper :: Text -> EType env (a -> b -> c) -> (a -> b -> c) -> Int -> UExpr env
    UVar :: Text -> EType env a -> (env -> a) -> UExpr env
    UAssign :: UExpr env -> UExpr env
    UApp :: UExpr env -> UExpr env -> UExpr env
    USeq :: UExpr env -> UExpr env -> UExpr env
    UIf :: UExpr env -> UExpr env -> UExpr env -> UExpr env
    UWhile :: UExpr env -> UExpr env -> UExpr env

instance Show (UExpr e) where
    show (UConst n t _) = "(" <> unpack n <> " : " <> show t <> ")"
    show (UOper n t _ p) = "(" <> unpack n <> " : " <> show t <> "/" <> show p <> ")"
    show (UVar n t _) = "(" <> unpack n <> " : " <> show t <> ")"
    show (UAssign f) = "ASGN(" <> show f <> ")"
    show (UApp f a) = "(" <> show f <> " <- " <> show a <> ")"
    show (USeq a b) = show a <> "; " <> show b
    show (UIf b y n) = "IF (" <> show b <> ") THEN (" <> show y <> ") ELSE (" <> show n <> ")"
    show (UWhile b a) = "WHILE (" <> show b <> ") DO (" <> show a <> ")"

mkUExpr :: UExprConstr env -> Text -> UExpr env
mkUExpr (CConst t a) n = UConst n t a
mkUExpr (COper t f p) n = UOper n t f p
mkUExpr (CVar t g) n = UVar n t g

-- We ignore the case of two operators applied together as it will fail type-
-- checking anyway.
precedence :: UExpr env -> UExpr env -> Int
precedence (UOper _ _ _ p) _ = p
precedence _ (UOper _ _ _ p) = p
precedence _ _ = 0
