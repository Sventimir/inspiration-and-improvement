{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Resolvers.Lexer (
    lexer
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Attoparsec.Text (Parser, choice, decimal, double, inClass, satisfy,
        skip, skipSpace, string, takeWhile)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Resolvers.Types (EType(..))
import Language.Resolvers.Unchecked (UExpr(..))

import Prelude hiding (takeWhile)
import Debug.Trace


instance Semigroup (UExpr env) where
    (UConst EUnit ()) <> e = e
    l <> r = USeq l r

instance Monoid (UExpr env) where
    mempty = UConst EUnit ()


lexer :: Map Text (UExpr env) -> Parser (UExpr env)
lexer prims = do
    skipSpace
    skip (== '{')
    skipSpace
    runReaderT (execWriterT parserLoop) $ Map.union primitives prims

parserLoop :: WriterT (UExpr env) (ReaderT (Map Text (UExpr env)) Parser) ()
parserLoop = do
    e <- lift $ choice [ expr Nothing ]
    tell (UAssign e)
    (lift . lift $ parseEnd) <|> parserLoop

parseEnd :: Parser ()
parseEnd = skipSpace >> skip (== '}') >> skipSpace

expr :: Maybe (UExpr env) -> ReaderT (Map Text (UExpr env)) Parser (UExpr env)
expr e = do
    trace (show e) $ return ()
    v <- choice [ name, subexpr, lift literal ]
    lift skipSpace
    choice [
            lift . endOfExpr $ maybeApp e v,
            expr $ Just (maybeApp e v)
        ]
    where
    name = lift (parseName <|> parseSymbol) >>= findName
    endOfExpr v = do
        skip (inClass ";)")
        skipSpace
        return v

subexpr :: ReaderT (Map Text (UExpr env)) Parser (UExpr env)
subexpr = do
    lift $ do
        skip (== '(')
        skipSpace
    e <- expr Nothing
    lift $ do
        skipSpace
        trace "Reaced )" $ return ()
    return e

literal :: Parser (UExpr env)
literal = choice [
        (string "()" >> return (UConst EUnit ())),
        fmap (UConst EInt) decimal,
        fmap (UConst EFloat) double
    ]

maybeApp :: Maybe (UExpr env) -> UExpr env -> UExpr env
maybeApp Nothing arg = arg
maybeApp (Just f) arg = UApp f arg

parseSymbol :: Parser Text
parseSymbol = takeWhile (\c -> not (inClass "a-zA-Z0-9_(){}[]" c || isSpace c))

parseName :: Parser Text
parseName = do
    init <- satisfy $ inClass "a-zA-Z_"
    rem <- takeWhile $ inClass "a-zA-Z0-9_"
    return (Text.cons init rem)

findName :: Text -> ReaderT (Map Text (UExpr env)) Parser (UExpr env)
findName name = do
    maybeVal <- asks $ Map.lookup name
    case maybeVal of
        Just v -> return v
        Nothing -> lift $ fail ("Undefined value: '" <> Text.unpack name <> "'.")

primitives :: Map Text (UExpr env)
primitives = Map.fromList [
        ("unit",    UConst EUnit ()),
        ("true",    UConst EBool True),
        ("false",   UConst EBool False),
        ("+",       UConst (EFun EInt (EFun EInt EInt)) (+)),
        ("-",       UConst (EFun EInt (EFun EInt EInt)) (-)),
        ("*",       UConst (EFun EInt (EFun EInt EInt)) (*))
    ]
