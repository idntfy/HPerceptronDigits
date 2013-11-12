module Neuron
    ( Neuron
    , create
    , modify
    , transfer
    , getInputsCount
    )
where

-- | Neuron with transfer function
type Neuron =
    ( [Int] -- synapse's weights
    , Int   -- threshold 
    )

-- | Transfer function
transfer :: Neuron -- neuron to apply
         -> [Int]  -- input data
         -> Int    -- output neuron value
transfer n xs = activator n $ adder n xs

-- | Actiovation function or Non-linear Converter
-- In this case it is Threshold function
activator :: Neuron -- neuron to apply
          -> Int    -- output of adder
          -> Int    -- normalized values
activator (_, t) s
    | s >= t = 1
    | otherwise = 0

-- | Adder
adder :: Neuron -- neuron to apply
      -> [Int]  -- input data
      -> Int    -- unweighted summ
adder (ws, _) xs = sum $ zipWith (*) xs ws

-- | Neuron initialization
create :: Int    -- synapse count
       -> Int    -- threshhold
       -> Neuron -- created neuron
create n t = (weights, t)
    where weights = take n (repeat 0)

-- | Synapse's weights modification
-- Using for teaching
modify :: Neuron -- neuron to apply
       -> Int    -- teach speed
       -> Int    -- difference between neuron's output and target output
       -> [Int]  -- input data
       -> Neuron -- modified neuron
modify (ws, t) v d xs = (weights, t)
    where weights = zipWith (+) ws $ map (d * v *) xs -- wi =+ v*d*xi

-- | Gets neuron's input count
getInputsCount :: Neuron -- applying neuron
               -> Int    -- input count
getInputsCount (ws, _) = length ws