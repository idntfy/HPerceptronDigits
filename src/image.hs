module Image
    ( ImagePath
    , getImagePaths
    , getImageList
    , getImageOrError
    , getImageBytes
    )
where

import Codec.Image.STB
import Data.ByteString (unpack)
import Data.List
import Data.Word
import System.Directory

type ImagePath = (Image, FilePath)

getImagePaths :: FilePath -> IO [FilePath]
getImagePaths dir = do
    fileList <- getDirectoryContents dir
    return (filter jpgFilter fileList)

jpgFilter :: FilePath -> Bool
jpgFilter = isSuffixOf ".jpg"

getImageList :: [FilePath] -> IO [ImagePath]
getImageList paths = do
    images <- mapM getImageOrError paths
    return (map extractImage $ filter filterImage images)
        where
            filterImage (image, path) = isRight image
            extractImage (Right image, path) = (image, path)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

getImageOrError :: FilePath -> IO (Either String Image, FilePath)
getImageOrError path = do
    imageOrError <- loadImage path
    return (imageOrError, path)

castWordToInt :: Word8 -> Int
castWordToInt = fromIntegral . toInteger

getImageBytes :: Image -> [Int]
getImageBytes = (map castWordToInt) . unpack . rawImage