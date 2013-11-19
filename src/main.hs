import Perceptron
import Teacher
import Control.Monad

import Codec.Image.STB
import Image

showResult (Left err) = show err
showResult (Right digit) = show digit

buildPath n = "../input/" ++ n ++ ".bmp"

recognizeAndPrint perceptron path = do
    result <- recognizeImageFile perceptron path
    putStr "Recognized as "
    putStrLn $ showResult result

main = do
    let path = "../data/"
    let neurons = 3
    let inputs = 64 * 64

    perceptron <- createTrainedPerceptron neurons inputs 50 path
    recognizeProcess perceptron

recognizeProcess perceptron = do
    putStrLn "---"
    putStrLn "Input name of file to recognize: "

    path <- getLine
    recognizeAndPrint perceptron (buildPath path)

    putStrLn "Is it right? (y/n)"
    decision <- getLine
    
    if decision == "y"
        then recognizeProcess perceptron
        else do
            putStrLn "What is it?"
            num <- getLine
            p <- traineFromPath perceptron path (read num)
            recognizeProcess p