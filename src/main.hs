import Image
import qualified Perceptron as P
import Teacher
import System.Directory
import Codec.Image.STB
import System.FilePath.Posix

showResult (Left err) = show err
showResult (Right digit) = show digit

main = do
    let path = "../data/"
    let neurons = 10
    let inputs = 64 * 64

    let buildPath n = "../input/" ++ n ++ ".jpg"
    let pathsToRecognize = map (buildPath . show) [0..9]

    perceptron <- createTrainedPerceptron neurons inputs 25 path

    mapM (recognizeAndPrint perceptron) pathsToRecognize

    return ()

recognizeAndPrint perceptron path = do
    result <- P.recognizeImageFile perceptron path
    print $ showResult result