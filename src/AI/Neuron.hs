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
    make :: Floating a => a -> [a] -> n a
    weights :: n a -> [a]
    bias :: n a -> a
    decide :: Floating a => n a -> [a] -> a

class Network n where
    eval :: Floating a => n a -> [a] -> [a]
    networkParser :: Floating a => Parser a -> Parser (n a)
    saveNetwork :: Show a => Handle -> n a -> IO ()

readNetwork :: (Floating a, Network n) =>  Handle -> Parser a -> EitherT String IO (n a)
readNetwork file weightParser = do
    c <- liftIO $ hGetChunk file
    r <- liftIO $ parseWith (hGetChunk file) (networkParser weightParser) c
    hoistEither $ eitherResult r
