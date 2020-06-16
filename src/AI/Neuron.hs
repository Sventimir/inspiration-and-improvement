module AI.Neuron (
    Neuron(..),
    Network(..),
    readNetwork
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Attoparsec.Text (Parser, parseWith, eitherResult)
import qualified Data.Text as Text
import Data.Text.IO (hGetChunk)
import System.IO (Handle)


class Neuron n where
    make :: (Floating a, Ord a) => a -> [a] -> n a
    weights :: n a -> [a]
    bias :: n a -> a
    decide :: Floating a => n a -> [a] -> a

class Network n where
    eval :: (Floating a, Ord a) => n a -> [a] -> [a]
    networkParser :: (Floating a, Ord a) => Parser a -> Parser (n a)
    saveNetwork :: Show a => Handle -> n a -> IO ()

readNetwork :: (Floating a, Ord a, Network n) =>  Handle -> Parser a -> EitherT String IO (n a)
readNetwork file weightParser = do
    c <- liftIO $ hGetChunk file
    r <- liftIO $ parseWith (hGetChunk file) (networkParser weightParser) c
    hoistEither $ eitherResult r

-- A cost function
meanSquaredError :: Floating a => [a] -> [a] -> a
meanSquaredError expected actual =
    sum . map (** 2) $ zipWith (-) expected actual

meanSquareErrorForSet :: Floating a => [[a]] -> [[a]] -> a
meanSquareErrorForSet expected actual =
    (sum $ zipWith meanSquaredError expected actual) / (2 * len)
    where
    len = fromIntegral $ min (length expected) (length actual)
