{-# LANGUAGE MultiParamTypeClasses #-}
module AI.Neuron.Perceptron (
    Perceptron,
    BinaryPerceptron,
    SigmoidPerceptron
) where

import AI.Neuron (Neuron(..))
import Control.Monad.State (StateT, get, put)
import Control.Monad.State.Compose (Wrapper(..), wrapped)


data Perceptron a = Perceptron {
    _activation :: a -> a,
    _weights :: [a],
    _bias, _value :: a
}


eval :: Num a => Perceptron a -> [a] -> a
eval (Perceptron activate weights bias _) inputs =
    activate $ sum (zipWith (*) weights inputs) + bias

evalFire :: (Num a, Monad m) => [a] -> StateT (Perceptron a) m a
evalFire inputs = do
    p <- get
    let output = eval p inputs
    put $ p { _value = output }
    return output

newtype BinaryPerceptron a = BinaryPerceptron (Perceptron a)

instance Wrapper (BinaryPerceptron a) (Perceptron a) where
    wrap = BinaryPerceptron
    unwrap (BinaryPerceptron p) = p

instance Neuron BinaryPerceptron where
    decide (BinaryPerceptron p) = eval p
    weights (BinaryPerceptron (Perceptron _ ws _ _)) = ws
    bias (BinaryPerceptron (Perceptron _ _ b _)) = b
    value (BinaryPerceptron (Perceptron _ _ _ v)) = v
    fire inputs = wrapped $ evalFire inputs
    make bias weights = BinaryPerceptron $ Perceptron rnd weights bias 0
        where
        rnd x = if x > 0.5 then 1 else 0

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- Derrivative of the sigmoid function.
sigmoid' :: Floating a => a -> a
sigmoid' x = let s = sigmoid x in s * (1 - s)

newtype SigmoidPerceptron a = SigmoidPerceptron (Perceptron a)

instance Wrapper (SigmoidPerceptron a) (Perceptron a) where
    wrap = SigmoidPerceptron
    unwrap (SigmoidPerceptron p) = p

instance Neuron SigmoidPerceptron where
    decide (SigmoidPerceptron p) = eval p
    weights (SigmoidPerceptron (Perceptron _ ws _ _)) = ws
    bias (SigmoidPerceptron (Perceptron _ _ b _)) = b
    value (SigmoidPerceptron (Perceptron _ _ _ v)) = v
    fire inputs = wrapped $ evalFire inputs
    make bias weights = SigmoidPerceptron $ Perceptron sigmoid weights bias 0
