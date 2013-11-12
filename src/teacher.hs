module Teacher
    ( teachFromDirectory
    , teachFromImage
    , createTrainedPerceptron
    )
where

import Codec.Image.STB
import Data.Either
import System.FilePath.Posix
import Image
import qualified Perceptron as P

-- | Predicate
-- Returns true if width*height of image less
-- than perceptron inputs
imageSizeFilter :: P.Perceptron
                -> ImagePath
                -> Bool
imageSizeFilter p (img, _) = w * h <= P.getInputsCount p
    where (w, h) = resolution img

-- | Returns list which accordings to target value
-- It has 1 on position of number which accordings to it
-- All other values is 0.
getOutVector :: P.Perceptron -- target perceptron
             -> Int          -- target number
             -> [Int]        -- list which accordings to number
getOutVector p n = replaceAtIndex n 1 empty
    where
        empty = take (P.getNeuronsCount p) (repeat 0)
        replaceAtIndex n item xs
            | n >= length xs = xs
            | otherwise = take n xs ++ [item] ++ drop (n + 1) xs

-- | Traine existing perceptron by concrete image
-- Image name should has format {targetNumber}.{whatever}.jpg
teachFromImage :: P.Perceptron -- perceptron to teach
               -> ImagePath    -- image sample
               -> P.Perceptron -- trained perceptron
teachFromImage p (img, path) = P.teach p (buildInVector img) (buildOutVector path)
    where
        buildInVector = P.normalizeImageData . getImageBytes
        buildOutVector = getOutVector p . read . takeWhile (/= '.') . takeFileName

-- | Traine existing perceptron by images in path
-- Image names in directory should has format {targetNumber}.{whatever}.jpg
teachFromDirectory :: P.Perceptron    -- perceptron to teach
                   -> Int             -- teaching cycles
                   -> FilePath        -- directory with images
                   -> IO P.Perceptron -- trained perceptron
teachFromDirectory p 0 _ = return p
teachFromDirectory p times path = do
    imageList <- getImagePaths path >>= getImageList
    let filteredList = filter (imageSizeFilter p) imageList
    let trained = foldl teachFromImage p filteredList
    teachFromDirectory trained (times - 1) path

-- | Creates perceptron which trained by images in path
-- Image names in directory should has format {targetNumber}.{whatever}.jpg
createTrainedPerceptron :: Int             -- neurons count
                        -> Int             -- inputs count
                        -> Int             -- teaching cycles
                        -> FilePath        -- directory with images
                        -> IO P.Perceptron -- created trained perceptron
createTrainedPerceptron neurons inputs times path = teachFromDirectory p times path
    where p = P.create neurons inputs