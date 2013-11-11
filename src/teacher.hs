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

imageSizeFilter :: P.Perceptron -> ImagePath -> Bool
imageSizeFilter p (img, _) = w * h <= P.getInputsCount p
    where (w, h) = resolution img

getInVector :: [Int] -> [Int]
getInVector = map f
    where
        f -1 = 0
        f _  = 1

getOutVector :: P.Perceptron -> Int -> [Int]
getOutVector p n = a ++ b ++ c
    where
        a = replicate (n - 1) 0
        b = [1]
        c = replicate (P.getNeuronsCount p - n) 0

teachFromImage :: P.Perceptron -> ImagePath -> P.Perceptron
teachFromImage p (img, path) = P.teach p (buildInVector img) (buildOutVector path)
    where
        buildInVector = getInVector . getImageBytes
        buildOutVector = getOutVector p . read . takeWhile (/= '.') . takeFileName

teachFromDirectory :: P.Perceptron -> FilePath -> IO P.Perceptron
teachFromDirectory p path = do
    imageList <- getImagePaths path >>= getImageList
    let filteredList = filter (imageSizeFilter p) imageList
    return (foldl teachFromImage p filteredList)

createTrainedPerceptron :: Int -> Int -> FilePath -> IO P.Perceptron
createTrainedPerceptron neurons inputs path = teachFromDirectory p path
    where p = P.create neurons inputs