module Neuron
    ( Neuron
    , create
    , modify
    , transfer
    , getInputsCount
    )
where

type Neuron = ([Int], Int)

transfer :: Neuron -> [Int] -> Int
transfer n xs = activator n $ adder n xs

activator :: Neuron -> Int -> Int
activator (_, t) s
    | s >= t = 1
    | otherwise = 0

adder :: Neuron -> [Int] -> Int
adder (ws, _) xs = sum $ zipWith (*) xs ws

create :: Int -> Int -> Neuron
create n t = (weights, t)
    where weights = take n (repeat 0)

modify :: Neuron -> Int -> Int -> [Int] -> Neuron
modify (ws, t) v d xs = (weights, t)
    where weights = zipWith (+) ws $ map (d * v *) xs

getInputsCount :: Neuron -> Int
getInputsCount (ws, _) = length ws