import Image
import Perceptron
import Teacher

main = do
    let path = "../data/"
    let pathToRecognize = "../data/recognize.jpg"
    let neurons = 10
    let inputs = 10

    perceptron <- createTrainedPerceptron neurons inputs path
    print perceptron
    return ()
