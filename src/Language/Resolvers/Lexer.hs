{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Resolvers.Lexer (
    lexer
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Attoparsec.Text (Parser, choice, decimal, inClass, satisfy, skip,
        skipSpace, takeWhile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Resolvers.Types (EType(..))
import Language.Resolvers.Unchecked (UExpr(..))

import Prelude hiding (takeWhile)


instance Semigroup (UExpr env) where
    (UConst EUnit ()) <> e = e
    l <> r = USeq l r

instance Monoid (UExpr env) where
    mempty = UConst EUnit ()


lexer :: Map Text (UExpr env) -> Parser (UExpr env)
lexer primitives = do
    skipSpace
    skip (== '{')
    skipSpace
    runReaderT (execWriterT parserLoop) primitives

parserLoop :: WriterT (UExpr env) (ReaderT (Map Text (UExpr env)) Parser) ()
parserLoop = do
    e <- lift $ choice [ expr Nothing ]
    tell (UAssign e)
    (lift . lift $ parseEnd) <|> parserLoop

parseEnd :: Parser ()
parseEnd = skipSpace >> skip (== '}') >> skipSpace

expr :: Maybe (UExpr env) -> ReaderT (Map Text (UExpr env)) Parser (UExpr env)
expr e = do
    v <- choice [ name, lift intLit ]
    lift skipSpace
    choice [
            lift . endOfExpr $ maybeApp e v,
            expr $ Just (maybeApp e v)
        ]
    where
    name = lift parseName >>= findName
    intLit = fmap (UConst EInt) decimal
    endOfExpr v = do
        skip (== ';')
        skipSpace
        return v

maybeApp :: Maybe (UExpr env) -> UExpr env -> UExpr env
maybeApp Nothing arg = arg
maybeApp (Just f) arg = UApp f arg

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
