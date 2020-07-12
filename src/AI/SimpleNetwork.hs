{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, GADTs #-}
module AI.SimpleNetwork (
    SimpleNetwork(..)
) where

import AI.Neuron

import Control.Applicative ((<|>), optional)
import Control.Monad.Combinators (many)
import Control.Monad.State (StateT)
import Control.Monad.State.Compose (Wrapper(..), chainState, forEachState, wrapped)

import qualified Data.Text as Text
import Data.Text.IO (hPutStr, hPutStrLn)

import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char as Megaparsec
import System.IO (Handle)


data SimpleNetwork n a where
    SimpleNetwork :: Neuron n => [[n a]] -> SimpleNetwork n a

instance Neuron n => Wrapper (SimpleNetwork n a) [[n a]] where
    wrap = SimpleNetwork
    unwrap (SimpleNetwork ls) = ls

instance Neuron n => Network (SimpleNetwork n) where
    feed inputs = wrapped $ chainState feedForward inputs
        where
        feedForward :: (Floating a, Monad m) => [a] -> StateT [n a] m [a]
        feedForward inputs = do forEachState (fire inputs)

    networkParser weight = do
        optional $ Megaparsec.char '^'
        layers <- many $ parseLayer weight
        return $ SimpleNetwork layers

    saveNetwork file (SimpleNetwork layers) = writeNetwork file layers

    outputs (SimpleNetwork []) = []
    outputs (SimpleNetwork layers) =
        map value $ last layers


writeNetwork :: (Neuron n, Show a) => Handle -> [[n a]] -> IO ()
writeNetwork _ [] = return ()
writeNetwork file (layer : more) = do
    mapM (hPutStr file . Text.intercalate "\t" . map (Text.pack . show) . weights) layer
    hPutStrLn file "^"
    writeNetwork file more

naiveNetworkParser :: (Floating a, Ord a) => Neuron n => Parser a -> Parser (SimpleNetwork n a)
naiveNetworkParser weight = do
    optional $ Megaparsec.char '^'
    layers <- many $ parseLayer weight
    return $ SimpleNetwork layers

parseLayer :: (Floating a, Ord a) => Neuron n => Parser a -> Parser [n a]
parseLayer weight = do
    neurons <- many $ parseNeuron weight
    Megaparsec.char '^'
    Megaparsec.space
    return neurons

parseNeuron :: (Floating a, Ord a) => Neuron n => Parser a -> Parser (n a)
parseNeuron weight = do
    b <- weight
    ws <- many $ do
        Megaparsec.char ' ' <|> Megaparsec.char '\t'
        weight
    Megaparsec.eol
    return $ make b ws
