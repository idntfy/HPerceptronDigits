module Neuron
    ( Neuron
    , create
    , modify
    , transfer
    , getInputsCount
    )
where

data Neuron = Neuron { weights :: [Int]
                     , threshold :: Int
                     } deriving (Show, Eq)

transfer :: Neuron -> [Int] -> Int
transfer n xs = activator n $ adder n xs

activator :: Neuron -> Int -> Int
activator (Neuron _ t) s
    | s >= t = 1
    | otherwise = 0

adder :: Neuron -> [Int] -> Int
adder (Neuron ws _) xs = sum $ zipWith (*) xs ws

create :: Int -> Int -> Neuron
create n t = Neuron weights t
    where weights = take n (repeat 0)

modify :: Neuron -> Int -> Int -> [Int] -> Neuron
modify (Neuron ws t) v d xs = Neuron weights t
    where weights = zipWith (+) ws $ map (d * v *) xs

getInputsCount :: Neuron -> Int
getInputsCount (Neuron ws _) = length ws