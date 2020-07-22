{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Language.Resolvers.Parser (
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
import Language.Resolvers.Unchecked (Loc, UExpr(..), UExprConstr(..), mkUExpr, location)

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
    position <- fmap Megaparsec.stateOffset Megaparsec.getParserState
    expr <- sepBy exprParser $ symbol ";"
    return $ case expr of
        [] -> UConst (position, position) "()" EUnit ()
        es -> foldl1 USeq $ map UAssign es

exprParser :: Monad m => ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
exprParser = flip Parser.makeExprParser operators $ Megaparsec.choice [
        Megaparsec.between (symbol "(") (symbol ")") exprParser,
        Megaparsec.try termParser,
        lift literal
    ]

termParser :: Monad m => ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
termParser = lexeme (withPosition parseName >>= findName)
    where
    parseName = do
        c <- Megaparsec.letterChar
        cs <- fmap Text.pack $ many Megaparsec.alphaNumChar
        return (Text.cons c cs)

literal :: Monad m => Parser m (UExpr env)
literal = lexeme $ Megaparsec.choice [
        Megaparsec.try (mkUnit <$> (withPosition $ symbol "()")),
        Megaparsec.try (mkLit EFloat <$> literalParser Lexer.float),
        mkLit EInt <$> literalParser Lexer.decimal
    ]
    where
    mkUnit :: (a, Loc) -> UExpr env
    mkUnit (_, loc) = UConst loc "()" EUnit ()
    mkLit :: EType env a -> (a, Text, Loc) -> UExpr env
    mkLit t (lit, txt, loc) = UConst loc txt t lit

literalParser :: Parser m a -> Parser m (a, Text, Loc)
literalParser p = do
    (lit, (bgn, end)) <- Megaparsec.lookAhead $ withPosition p
    txt <- Megaparsec.takeP Nothing (end - bgn)
    return (lit, txt, (bgn, end))


findName :: Monad m => (Text, Loc) -> ReaderT (Map Text (UExprConstr env)) (Parser m) (UExpr env)
findName (name, loc) = do
    maybeVal <- asks $ Map.lookup name
    case maybeVal of
        Just constr -> return $ mkUExpr constr loc name
        Nothing -> lift $ fail ("Undefined value: '" <> Text.unpack name <> "'.")

operators :: MonadParsec e Text m => [[Operator m (UExpr env)]]
operators = [
        [ InfixL (applyOperator "+" (EFun EInt (EFun EInt EInt)) (+)),
          InfixL (applyOperator "-" (EFun EInt (EFun EInt EInt)) (-))
        ],
        [ InfixL (applyOperator "*" (EFun EInt (EFun EInt EInt)) (*)) ],
        [ InfixL apply ]
    ]

apply :: MonadParsec e Text m => m (UExpr env -> UExpr env -> UExpr env)
apply = do
    symbol ""
    return $ \a b -> UApp (fst $ location a, snd $ location b) a b

applyOperator :: MonadParsec e Text m => Text -> EType env (a -> b -> c) -> (a -> b -> c) -> m (UExpr env -> UExpr env -> UExpr env)
applyOperator s t f =
    fmap snd (withPosition $ symbol s) >>= \(bgnOp, endOp) -> return $ \a b ->
    let (bgnA, _) = location a
        (_, endB) = location b
        op = UConst (bgnOp, endOp) ("(" <> s <> ")") t f
    in
    UApp (bgnOp, endB) (UApp (bgnA, endOp) op a) b

primitives :: Map Text (UExprConstr env)
primitives = Map.fromList [
        ("unit",    CConst EUnit ()),
        ("true",    CConst EBool True),
        ("false",   CConst EBool False)
    ]

withPosition :: MonadParsec e Text m => m a -> m (a, Loc)
withPosition p = do
    bgn <- fmap Megaparsec.stateOffset Megaparsec.getParserState
    a <- p
    end <- fmap Megaparsec.stateOffset Megaparsec.getParserState
    return (a, (bgn, end))
