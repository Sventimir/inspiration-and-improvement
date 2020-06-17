import AI.Neuron
import AI.Neuron.Perceptron
import AI.SimpleNetwork
import Control.Monad.State (evalStateT)

main :: IO ()
main = do
    putStrLn "Exercises:"
    digitToBin

digitToBin :: IO ()
digitToBin = mapM_ displayResult [0..9]
    where
    displayResult d = do
        output <- evalStateT (feed $ vectorizeDigit d) digitNet
        putStrLn ("Result for digit " ++ show d ++ " is: " ++ show output)
    vectorizeDigit 0 = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    vectorizeDigit 1 = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
    vectorizeDigit 2 = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
    vectorizeDigit 3 = [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]
    vectorizeDigit 4 = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
    vectorizeDigit 5 = [0, 0, 0, 0, 0, 1, 0, 0, 0, 0]
    vectorizeDigit 6 = [0, 0, 0, 0, 0, 0, 1, 0, 0, 0]
    vectorizeDigit 7 = [0, 0, 0, 0, 0, 0, 0, 1, 0, 0]
    vectorizeDigit 8 = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0]
    vectorizeDigit 9 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    vectorizeDigit _ = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    digitNet = SimpleNetwork [[
            (make 0 [0, 0, 0, 0, 0, 0, 0, 0, 1, 1] :: BinaryPerceptron Double),
            make 0 [0, 0, 0, 0, 1, 1, 1, 1, 0, 0],
            make 0 [0, 0, 1, 1, 0, 0, 1, 1, 0, 0],
            make 0 [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
        ]]
