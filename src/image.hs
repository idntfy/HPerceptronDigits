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
import System.FilePath.Posix

type ImagePath = (Image, FilePath)

-- | Gets list of paths of images from dir
getImagePaths :: FilePath -> IO [FilePath]
getImagePaths dir = do
    fileList <- getDirectoryContents dir
    return (buildImagePaths dir fileList)

-- | Build full path list from list of images and dir path
buildImagePaths :: FilePath -> [FilePath] -> [FilePath]
buildImagePaths dir = filter jpgFilter . map (combine dir)

-- | Predicate
-- Returns true if file has ".jpg" extension
jpgFilter :: FilePath -> Bool
jpgFilter = (== ".jpg") . takeExtension

-- | Returns list of images from list of some files
getImageList :: [FilePath] -> IO [ImagePath]
getImageList paths = do
    images <- mapM getImageOrError paths
    return (map extractImage $ filter filterImage images)
        where
            filterImage (image, path) = isRight image
            extractImage (Right image, path) = (image, path)

-- | Tests that Either is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Returns image or error
-- Returns tuple of image and path
getImageOrError :: FilePath -> IO (Either String Image, FilePath)
getImageOrError path = do
    imageOrError <- loadImage path
    return (imageOrError, path)

-- | Converts 8-bit word to Int
castWordToInt :: Word8 -> Int
castWordToInt = fromIntegral . toInteger

-- | Returns image bytes conveted to Int
getImageBytes :: Image -> [Int]
getImageBytes = (map castWordToInt) . unpack . rawImage