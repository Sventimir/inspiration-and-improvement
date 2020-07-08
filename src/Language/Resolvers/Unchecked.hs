{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Resolvers.Unchecked (
    UExpr(..),
    UExprConstr(..),
    mkUExpr,
    precedence
) where

import Control.Monad.Trans (lift)
import Control.Monad.Writer (Writer, WriterT, execWriter, runWriterT, tell)

import Data.List (nub, sortOn)
import Data.Ord (Down(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)

import Language.Resolvers.Types (EType(..))

import Debug.Trace


type Precedence = Int

data UExprConstr env where
    CConst :: EType env a -> a -> UExprConstr env
    COper :: EType env (a -> b -> c) -> (a -> b -> c) -> Precedence -> UExprConstr env
    CVar :: EType env a -> (env -> a) -> UExprConstr env

data UExpr env where
    UConst :: Text -> EType env a -> a -> UExpr env
    UOper :: Text -> EType env (a -> b -> c) -> (a -> b -> c) -> Precedence -> UExpr env
    UVar :: Text -> EType env a -> (env -> a) -> UExpr env
    UAssign :: UExpr env -> UExpr env
    UApp :: UExpr env -> UExpr env -> UExpr env
    USeq :: UExpr env -> UExpr env -> UExpr env
    UIf :: UExpr env -> UExpr env -> UExpr env -> UExpr env
    UWhile :: UExpr env -> UExpr env -> UExpr env

instance Show (UExpr e) where
    show (UConst n _ _) = unpack n
    show (UOper n _ _ _) = unpack n
    show (UVar n _ _) = unpack n
    show (UAssign f) = "ASSIGN(" <> show f <> ")"
    show (UApp f a) = "(" <> show f <> " " <> show a <> ")"
    show (USeq a b) = show a <> "; " <> show b
    show (UIf b y n) = "IF (" <> show b <> ") THEN (" <> show y <> ") ELSE (" <> show n <> ")"
    show (UWhile b a) = "WHILE (" <> show b <> ") DO (" <> show a <> ")"


showFull :: UExpr env -> String
showFull (UConst n t _) = "(" <> unpack n <> " : " <> show t <> ")"
showFull (UOper n t _ p) = "(" <> unpack n <> " : " <> show t <> "/" <> show p <> ")"
showFull (UVar n t _) = "(" <> unpack n <> " : " <> show t <> ")"
showFull (UAssign f) = "ASSIGN(" <> showFull f <> ")"
showFull (UApp f a) = "(" <> showFull f <> " <- " <> showFull a <> ")"
showFull (USeq a b) = showFull a <> "; " <> showFull b
showFull (UIf b y n) = "IF (" <> showFull b <> ") THEN (" <> showFull y <> ") ELSE (" <> showFull n <> ")"
showFull (UWhile b a) = "WHILE (" <> showFull b <> ") DO (" <> showFull a <> ")"

mkUExpr :: UExprConstr env -> Text -> UExpr env
mkUExpr (CConst t a) n = UConst n t a
mkUExpr (COper t f p) n = UOper n t f p
mkUExpr (CVar t g) n = UVar n t g

-- We ignore the case of two operators applied together as it will fail type-
-- checking anyway.
precedence :: UExpr env -> UExpr env -> Precedence
precedence (UOper _ _ _ p) _ = p
precedence _ (UOper _ _ _ p) = p
precedence (UApp (UOper _ _ _ p) _) _ = p
precedence _ _ = 0
