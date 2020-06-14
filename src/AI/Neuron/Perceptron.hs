module AI.Neuron.Perceptron (
    Perceptron,
    BinaryPerceptron,
    SigmoidPerceptron
) where

import AI.Neuron (Neuron(..))


data Perceptron a = Perceptron (a -> a) [a] a

eval :: Num a => Perceptron a -> [a] -> a
eval (Perceptron activate weights bias) inputs =
    activate $ sum (zipWith (*) weights inputs) + bias

newtype BinaryPerceptron a = BinaryPerceptron (Perceptron a)

instance Neuron BinaryPerceptron where
    decide (BinaryPerceptron p) = eval p
    weights (BinaryPerceptron (Perceptron _ ws _)) = ws
    bias (BinaryPerceptron (Perceptron _ _ b)) = b
    make bias weights = BinaryPerceptron $ Perceptron rnd weights bias
        where
        rnd x = if x > 0.5 then 1 else 0

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- Derrivative of the sigmoid function.
sigmoid' :: Floating a => a -> a
sigmoid' x = let s = sigmoid x in s * (1 - s)

newtype SigmoidPerceptron a = SigmoidPerceptron (Perceptron a)

instance Neuron SigmoidPerceptron where
    decide (SigmoidPerceptron p) = eval p
    weights (SigmoidPerceptron (Perceptron _ ws _)) = ws
    bias (SigmoidPerceptron (Perceptron _ _ b)) = b
    make bias weights = SigmoidPerceptron $ Perceptron sigmoid weights bias
