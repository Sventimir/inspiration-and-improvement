{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Language.Resolvers.Lexer (
    Parser,
    body,
    lexeme,
    space,
    symbol
) where

import Control.Applicative ((<|>), (<$), optional)
import Control.Monad.Combinators (many, sepBy)
import Control.Monad.Combinators.Expr (Operator(..))
import qualified Control.Monad.Combinators.Expr as Parser
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)

import Language.Resolvers.Types (EType(..))
import Language.Resolvers.Unchecked (UExpr(..), UExprConstr(..), mkUExpr)

import Text.Megaparsec (MonadParsec, ParsecT)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Prelude hiding (takeWhile)
import Debug.Trace


type Parser = ParsecT Void Text

space :: MonadParsec e Text m => m ()
space = Lexer.space
    Megaparsec.space1
    (Lexer.skipLineComment "#")
    (Lexer.skipBlockComment "{-" "-}")

symbol :: MonadParsec e Text m => Text -> m Text
symbol = Lexer.symbol space

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = Lexer.lexeme space

body :: Monad m => Map Text (UExprConstr env) -> Parser m (UExpr env)
body prims = Megaparsec.between (symbol "{") (symbol "}") $
        runReaderT langParser $ Map.union primitives prims

langParser :: Monad m => ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
langParser = do
    expr <- sepBy exprParser $ symbol ";"
    return $ case expr of
        [] -> UConst "()" EUnit ()
        es -> foldl1 USeq $ map UAssign es

exprParser :: Monad m => ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
exprParser = flip Parser.makeExprParser operators $ Megaparsec.choice [
        Megaparsec.between (symbol "(") (symbol ")") exprParser,
        Megaparsec.try termParser,
        lift literal
    ]

termParser :: Monad m => ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
termParser = lexeme $ do
    c <- Megaparsec.letterChar
    cs <- fmap Text.pack $ many Megaparsec.alphaNumChar
    findName (Text.cons c cs)

literal :: Monad m => Parser m (UExpr env)
literal = lexeme $ Megaparsec.choice [
        (Megaparsec.string "()" >> return (UConst "()" EUnit ())),
        fmap (\lit -> UConst (Text.pack $ show lit) EInt lit) Lexer.decimal,
        fmap (\lit -> UConst (Text.pack $ show lit) EFloat lit) Lexer.float
    ]

findName :: Monad m => Text -> ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
findName name = do
    maybeVal <- asks $ Map.lookup name
    case maybeVal of
        Just constr -> return $ mkUExpr constr name
        Nothing -> lift $ fail ("Undefined value: '" <> Text.unpack name <> "'.")

operators :: MonadParsec e Text m => [[Operator m (UExpr env)]]
operators = [
        [ InfixL (app "+" $ UConst "(+)" (EFun EInt (EFun EInt EInt)) (+)),
          InfixL (app "-" $ UConst "(-)" (EFun EInt (EFun EInt EInt)) (-))
        ],
        [ InfixL (app "*" $ UConst "(*)" (EFun EInt (EFun EInt EInt)) (*)) ],
        [ InfixL (UApp <$ symbol "") ]
    ]
    where
    app :: MonadParsec e Text m => Text -> UExpr env -> m (UExpr env -> UExpr env -> UExpr env)
    app s op = (\a b -> UApp (UApp op a) b) <$ symbol s

primitives :: Map Text (UExprConstr env)
primitives = Map.fromList [
        ("unit",    CConst EUnit ()),
        ("true",    CConst EBool True),
        ("false",   CConst EBool False)
    ]
