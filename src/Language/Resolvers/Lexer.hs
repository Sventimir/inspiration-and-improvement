{-# LANGUAGE GADTs, OverloadedStrings #-}
module Language.Resolvers.Lexer (
    lexer
) where

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Attoparsec.Text (Parser, choice, inClass, satisfy, skip, skipSpace, takeWhile)
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
lexer = runReaderT $ do
    lift $ do
        skipSpace
        skip (== '{')
        skipSpace
    e <- execWriterT parserLoop
    lift $ do
        skipSpace
        skip (== '}')
        skipSpace
    return e

parserLoop :: WriterT (UExpr env) (ReaderT (Map Text (UExpr env)) Parser) ()
parserLoop = do
    e <- choice [ lift (expr Nothing) ]
    tell e
    parserLoop


expr :: Maybe (UExpr env) -> ReaderT (Map Text (UExpr env)) Parser (UExpr env)
expr e = do
    name <- lift parseName
    v <- findName name
    lift skipSpace
    choice [
            lift . endOfExpr $ maybeApp e v,
            expr $ Just (maybeApp e v)
        ]
    where
    endOfExpr v = do
        skip (== ';')
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
