{-# LANGUAGE OverloadedStrings #-}
module AI.Neuron (
    Neuron,
    NaiveNetwork(..),
    Network(..),
    sigmoid,
    sigmoidNeuron,
    readNetwork,
    writeNetwork
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Attoparsec.Text (Parser, eitherResult, parseWith, many', skip, skipSpace, endOfLine)
import qualified Data.Text as Text
import Data.Text.IO (hGetChunk, hPutStr, hPutStrLn)
import System.IO (Handle)


class Network n where
    eval :: Num a => n a -> [a] -> [a]
    networkParser :: Floating a => Parser a -> Parser (n a)
    saveNetwork :: Show a => Handle -> n a -> IO ()

data Neuron a = Neuron {
    activation :: a -> a,
    bias :: a,
    weights :: [a]
}

data NaiveNetwork a = OutputLayer
                    | NeuronLayer [Neuron a] (NaiveNetwork a)

instance Network NaiveNetwork where
    eval OutputLayer inputs = inputs
    eval (NeuronLayer neurons more) inputs =
        eval more $ map (evalNeuron inputs) neurons
        where
        evalNeuron ins (Neuron { bias = b, activation = act, weights = ws }) =
            act . (+ b) . sum $ zipWith (*) ins ws

    networkParser = naiveNetworkParser sigmoid
    saveNetwork = writeNetwork

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- Derrivative of the sigmoid function.
sigmoid' :: Floating a => a -> a
sigmoid' x = let s = sigmoid x in s * (1 - s)

sigmoidNeuron :: Floating a => a -> [a] -> Neuron a
sigmoidNeuron b ws = Neuron {
        bias = b,
        weights = ws,
        activation = sigmoid
    }

writeNetwork :: Show a => Handle -> NaiveNetwork a -> IO ()
writeNetwork _ OutputLayer = return ()
writeNetwork file (NeuronLayer neurons more) = do
    mapM printNeuron neurons
    hPutStrLn file "^"
    writeNetwork file more
    where
    printNeuron (Neuron { weights = [] }) = return ()
    printNeuron (Neuron { weights = (w : ws) }) = do
        printWeight w
        mapM (\w -> hPutStr file "\t" >> printWeight w) ws
        hPutStr file "\n"
    printWeight = hPutStr file . Text.pack . show

readNetwork :: (Floating a,Network n) =>  Handle -> Parser a -> EitherT String IO (n a)
readNetwork file weightParser = do
    c <- liftIO $ hGetChunk file
    r <- liftIO $ parseWith (hGetChunk file) (networkParser weightParser) c
    hoistEither $ eitherResult r

naiveNetworkParser :: (a -> a) -> Parser a -> Parser (NaiveNetwork a)
naiveNetworkParser act weight = do
    skip (== '^') <|> return ()
    layers <- many' $ parseLayer act weight
    return $ foldr NeuronLayer OutputLayer layers

parseLayer :: (a -> a) -> Parser a -> Parser [Neuron a]
parseLayer act weight = do
    neurons <- many' $ parseNeuron act weight
    skip (== '^')
    skipSpace
    return neurons

parseNeuron :: (a -> a) -> Parser a -> Parser (Neuron a)
parseNeuron act weight = do
    b <- weight
    ws <- many' $ do
        skip (flip elem [' ', '\t'])
        weight
    endOfLine
    return $ Neuron act b ws
