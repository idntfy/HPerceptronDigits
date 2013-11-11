module Teacher
    ( teachFromDirectory
    , teachFromImage
    )
where

import Codec.Image.STB
import Data.ByteString (unpack)
import Data.Either
import Data.List
import Data.Word
import System.Directory
import System.FilePath.Posix
import qualified Perceptron as P

type ImagePath = (Image, FilePath)

getImagePaths :: FilePath -> IO [FilePath]
getImagePaths dir = do
    fileList <- getDirectoryContents dir
    return (filter jpgFilter fileList)

jpgFilter :: FilePath -> Bool
jpgFilter = isSuffixOf ".jpg"

imageSizeFilter :: P.Perceptron -> ImagePath -> Bool
imageSizeFilter p (img, _) = w * h > P.getInputsCount p
    where (w, h) = resolution img

getImageList :: [FilePath] -> IO [ImagePath]
getImageList paths = do
    images <- mapM getImageOrError paths
    return (map extractImage images)
        where extractImage (Right image, path) = (image, path)

getImageOrError :: FilePath -> IO (Either String Image, FilePath)
getImageOrError path = do
    imageOrError <- loadImage path
    return (imageOrError, path)

castWordToInt :: Word8 -> Int
castWordToInt = fromIntegral . toInteger

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
        buildInVector = getInVector . (map castWordToInt) . unpack . rawImage
        buildOutVector = getOutVector p . read . takeFileName

teachFromDirectory :: P.Perceptron -> FilePath -> IO P.Perceptron
teachFromDirectory p path = do
    imageList <- getImagePaths path >>= getImageList
    return (foldl teachFromImage p imageList)