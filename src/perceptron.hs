module Perceptron
    ( Perceptron
    , create
    , recognize
    , teach
    , getNeuronsCount
    , getInputsCount
    , recognizeImageFile
    , normalizeImageData
    )
where

import Image
import qualified Neuron as N

-- | Single-layer perceptron it is a list of neurons
type Perceptron = [N.Neuron]

-- | Create perceptron
create :: Int        -- neurons count
       -> Int        -- inputs count
       -> Perceptron -- created perceptron
create n m = take n $ repeat (N.create m 10) -- 10 - init weight

-- | Image recognition
recognize :: Perceptron -- perceptron to apply
          -> [Int]      -- input data
          -> [Int]      -- output image
recognize p xs = map (flip N.transfer xs) p

-- | Perceptron teaching
teach :: Perceptron -- perceptron to apply
      -> [Int]      -- input data
      -> [Int]      -- target input data
      -> Perceptron -- trained perceptron
teach p xs ys
    | ts == ys = p               -- quit training if perceptron runs up to target output 
    | otherwise = teach ns xs ys -- continue training
        where
            v = 1                  -- speed of teaching
            ts = recognize p xs    -- current perceptron output
            ds = zipWith (-) ys ts -- difference between perceptron output and target output
            ns = map mn $ zip p ds -- teach all neurons with new diff and input
                where mn (n, d) = N.modify n v d xs

-- | Gets neurons count
getNeuronsCount :: Perceptron -- perceptron to apply
                -> Int        -- neurons count
getNeuronsCount = length

-- | Gets perceptron inputs count
getInputsCount :: Perceptron -- perceptron to apply
               -> Int        -- inputs count
getInputsCount [] = 0
getInputsCount p = N.getInputsCount (head p)

-- | Gets last index of 1 in neuron output
getDigit :: [Int] -- perceptron output
         -> Int   -- output digit
getDigit [] = -1
getDigit ys = (dec . length . dropWhile (== 0) . reverse) ys
    where dec n = n - 1

-- | Opens image file by path and recognizes it by perceptron
recognizeImageFile :: Perceptron             -- whos recognizes
                   -> FilePath               -- image path to recognize
                   -> IO (Either String Int) -- Error or recognized digit
recognizeImageFile p path = do
    imageOrError <- getImageOrError path
    return (buildReturn imageOrError)
    where
        buildReturn (Left err, _) = Left err
        buildReturn (Right img, _) = Right (recognizeImage img)
        recognizeImage = getDigit . recognize p . normalizeImageData . getImageBytes

-- | Normalizes image data
-- Converts white color to 0, other colors to 1
normalizeImageData :: [Int] -> [Int]
normalizeImageData = map f
    where
        f 255 = 0
        f _  = 1