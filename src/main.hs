import qualified Perceptron as P
import Teacher

main = do
    let path = "../data/"
    let pathToRecognize = "../input/3.jpg"
    let neurons = 10
    let inputs = 64 * 64

    perceptron <- createTrainedPerceptron neurons inputs path
    result <- P.recognizeImageFile perceptron pathToRecognize

    print $ case result of
        Left err -> show err
        Right digit -> show digit

    return ()