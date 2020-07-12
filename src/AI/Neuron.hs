module AI.Neuron (
    Neuron(..),
    Network(..),
    Parser,
    readNetwork
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Either.Extra (mapLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Void (Void)

import Text.Megaparsec (ParsecT, errorBundlePretty, runParserT)
import System.IO (FilePath, Handle)


type Parser = ParsecT Void Text IO

class Neuron n where
    make :: (Floating a, Ord a) => a -> [a] -> n a
    weights :: n a -> [a]
    bias :: n a -> a
    value :: n a -> a  -- the last value of activation
    decide :: Floating a => n a -> [a] -> a
    fire :: (Floating a, Monad m) => [a] -> StateT (n a) m a

class Network n where
    feed :: (Floating a, Ord a, Monad m) => [a] -> StateT (n a) m [a]
    outputs :: n a -> [a]
    networkParser :: (Floating a, Ord a) => Parser a -> Parser (n a)
    saveNetwork :: Show a => Handle -> n a -> IO ()

readNetwork :: (Floating a, Ord a, Network n) => FilePath -> Parser a -> EitherT String IO (n a)
readNetwork filename weightParser = do
    text <- liftIO $ TextIO.readFile filename
    r <- liftIO $ runParserT (networkParser weightParser) filename text
    hoistEither $ mapLeft errorBundlePretty r

-- A cost function
meanSquaredError :: Floating a => [a] -> [a] -> a
meanSquaredError expected actual =
    sum . map (** 2) $ zipWith (-) expected actual

meanSquareErrorForSet :: Floating a => [[a]] -> [[a]] -> a
meanSquareErrorForSet expected actual =
    (sum $ zipWith meanSquaredError expected actual) / (2 * len)
    where
    len = fromIntegral $ min (length expected) (length actual)
