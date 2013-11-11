module Perceptron
    ( Perceptron
    , create
    , recognize
    , teach
    , getNeuronsCount
    , getInputsCount
    , getDigit
    )
where

import Image
import qualified Neuron as Neuron

type Perceptron = [Neuron.Neuron]

create :: Int -> Int -> Perceptron
create n m = take n $ repeat (Neuron.create m 10)

recognize :: Perceptron -> [Int] -> [Int]
recognize p xs = map (flip Neuron.transfer xs) p

teach :: Perceptron -> [Int] -> [Int] -> Perceptron
teach p xs ys
    | ts == ys = p
    | otherwise = teach ns xs ys
        where
            v = 1
            ts = recognize p xs
            ds = zipWith (-) ys ts
            ns = map (\(n, d) -> Neuron.modify n v d xs) $ zip p ds

getNeuronsCount :: Perceptron -> Int
getNeuronsCount = length

getInputsCount :: Perceptron -> Int
getInputsCount [] = 0
getInputsCount p = Neuron.getInputsCount (head p)

getDigit :: [Int] -> Int
getDigit [] = -1
getDigit ys = last $ filter (1 ==) ys