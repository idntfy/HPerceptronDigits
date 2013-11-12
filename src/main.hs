import Perceptron
import Teacher
import Control.Monad

showResult (Left err) = show err
showResult (Right digit) = show digit

buildPath n = "../input/" ++ n ++ ".jpg"

recognizeAndPrint perceptron path = do
    result <- recognizeImageFile perceptron path
    putStr "Recognized as "
    putStrLn $ showResult result

main = do
    let path = "../data/"
    let neurons = 10
    let inputs = 64 * 64

    putStrLn "Traininig perceptron..."
    perceptron <- createTrainedPerceptron neurons inputs 25 path
    putStrLn "Perceptron trained."

    forever $ do 
        putStrLn "Input name of file to recognize: "
        fileName <- getLine
        recognizeAndPrint perceptron (buildPath fileName)