{-# LANGUAGE OverloadedStrings, GADTs #-}
module AI.Neuron (
    Neuron(..),
    Network(..),
    SimpleNetwork(..),
    readNetwork
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Attoparsec.Text (Parser, eitherResult, parseWith, many', skip, skipSpace, endOfLine)
import qualified Data.Text as Text
import Data.Text.IO (hGetChunk, hPutStr, hPutStrLn)
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

data SimpleNetwork n a where
    SimpleNetwork :: Neuron n => [[n a]] -> SimpleNetwork n a

instance Neuron n => Network (SimpleNetwork n) where
    eval (SimpleNetwork layers) inputs = feedForward layers inputs
        where
        feedForward [] inputs = inputs
        feedForward (l : ls) inputs =
            feedForward ls $ map (flip decide inputs) l

    networkParser weight = do
        skip (== '^') <|> return ()
        layers <- many' $ parseLayer weight
        return $ SimpleNetwork layers

    saveNetwork file (SimpleNetwork layers) = writeNetwork file layers


writeNetwork :: (Neuron n, Show a) => Handle -> [[n a]] -> IO ()
writeNetwork _ [] = return ()
writeNetwork file (layer : more) = do
    mapM (hPutStr file . Text.intercalate "\t" . map (Text.pack . show) . weights) layer
    hPutStrLn file "^"
    writeNetwork file more

readNetwork :: (Floating a, Network n) =>  Handle -> Parser a -> EitherT String IO (n a)
readNetwork file weightParser = do
    c <- liftIO $ hGetChunk file
    r <- liftIO $ parseWith (hGetChunk file) (networkParser weightParser) c
    hoistEither $ eitherResult r

naiveNetworkParser :: Floating a => Neuron n => Parser a -> Parser (SimpleNetwork n a)
naiveNetworkParser weight = do
    skip (== '^') <|> return ()
    layers <- many' $ parseLayer weight
    return $ SimpleNetwork layers

parseLayer :: Floating a => Neuron n => Parser a -> Parser [n a]
parseLayer weight = do
    neurons <- many' $ parseNeuron weight
    skip (== '^')
    skipSpace
    return neurons

parseNeuron :: Floating a => Neuron n => Parser a -> Parser (n a)
parseNeuron weight = do
    b <- weight
    ws <- many' $ do
        skip (flip elem [' ', '\t'])
        weight
    endOfLine
    return $ make b ws
