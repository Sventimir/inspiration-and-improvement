{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Resolvers.Unchecked (
    Loc,
    UExpr(..),
    UExprConstr(..),
    location,
    mkUExpr
) where

import Control.Monad.Trans (lift)
import Control.Monad.Writer (Writer, WriterT, execWriter, runWriterT, tell)

import Data.List (nub, sortOn)
import Data.Ord (Down(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)

import Language.Resolvers.Types (EType(..))


type Loc = (Int, Int)

data UExprConstr env where
    CConst :: EType env a -> a -> UExprConstr env
    CVar :: EType env a -> (env -> a) -> UExprConstr env

data UExpr env where
    UConst :: Loc -> Text -> EType env a -> a -> UExpr env
    UVar :: Loc -> Text -> EType env a -> (env -> a) -> UExpr env
    UAssign :: UExpr env -> UExpr env
    UApp :: Loc -> UExpr env -> UExpr env -> UExpr env
    USeq :: UExpr env -> UExpr env -> UExpr env
    UIf :: Loc -> UExpr env -> UExpr env -> UExpr env -> UExpr env
    UWhile :: Loc -> UExpr env -> UExpr env -> UExpr env
    UPair :: Loc -> UExpr env -> UExpr env -> UExpr env

instance Show (UExpr e) where
    show (UConst _ n _ _) = unpack n
    show (UVar _ n _ _) = unpack n
    show (UAssign f) = "ASSIGN(" <> show f <> ")"
    show (UApp _ f a) = "(" <> show f <> " " <> show a <> ")"
    show (USeq a b) = show a <> "; " <> show b
    show (UIf _ b y n) = "IF (" <> show b <> ") THEN (" <> show y <> ") ELSE (" <> show n <> ")"
    show (UWhile _ b a) = "WHILE (" <> show b <> ") DO (" <> show a <> ")"
    show (UPair _ a b) = "(" <> show a <> ", " <> show b <> ")"


showFull :: UExpr env -> String
showFull (UConst _ n t _) = "(" <> unpack n <> " : " <> show t <> ")"
showFull (UVar _ n t _) = "(" <> unpack n <> " : " <> show t <> ")"
showFull (UAssign f) = "ASSIGN(" <> showFull f <> ")"
showFull (UApp _ f a) = "(" <> showFull f <> " <- " <> showFull a <> ")"
showFull (USeq a b) = showFull a <> "; " <> showFull b
showFull (UIf _ b y n) = "IF (" <> showFull b <> ") THEN (" <> showFull y <> ") ELSE (" <> showFull n <> ")"
showFull (UWhile _ b a) = "WHILE (" <> showFull b <> ") DO (" <> showFull a <> ")"
showFill (UPair _ a b) = "(" <> showFull a <> ", " <> showFull b <> ")"

mkUExpr :: UExprConstr env -> Loc -> Text -> UExpr env
mkUExpr (CConst t a) l n = UConst l n t a
mkUExpr (CVar t g) l n = UVar l n t g

location :: UExpr env -> Loc
location (UConst l _ _ _) = l
location (UVar l _ _ _) = l
location (UAssign e) = location e
location (UApp l _ _) = l
location (USeq e _) = location e
location (UIf l _ _ _) = l
location (UWhile l _ _) = l
location (UPair l _ _) = l
