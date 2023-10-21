module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer
import Functions
import Network
import CsvReader
import System.Random
import System.Random.Shuffle

irisNet :: IO Network
irisNet = do
    l1 <- mkLayer sigmoid 5 4
    l2 <- mkLayer sigmoid 9 5
    l3 <- mkLayer sigmoid 5 9
    l4 <- mkLayer softmax 3 5
    return $ Network [l1, l2, l3, l4] 0.001 crossEntropy

runIris :: IO ()
runIris = do
    datas <- readCSV "Iris.csv"
    n2 <- irisNet
    gen <- newStdGen

    let
        datas' = map (init . tail) datas
        labels = map last datas

        inputs = map (map read) datas' :: [[Double]]
        targets = map toOneHot labels
            where
                toOneHot x = map (tobin . (==x)) tlab
                tobin b = if b then 1 else 0
                tlab = ["Iris-setosa", "Iris-versicolor", "Iris-virginica"]

        fullData = zip inputs targets

        shufData = shuffle' fullData (length fullData) gen
        trainData = map (\(x,y) -> (N.vector x, N.vector y)) $ take 140 shufData
        testData = map (\(x,y) -> (N.vector x, N.vector y)) $ drop 100 shufData

        err1 = test n2 testData

    let
        trained = train n2 $ concat (replicate 9999 trainData)
        err2 = test trained trainData

    putStrLn $ "Erro1 : " ++ show err1
    putStrLn $ "Erro2 : " ++ show err2


testNet :: Network
testNet = Network [l1, l2] 0.1 crossEntropy
    where
        l1 = mkPrefLayer sigmoid 2 3
        l2 = mkPrefLayer softmax 2 2

runTest :: IO ()
runTest = do
    let err1 = test testNet [(input, target)]
    print err1
    let trained = train testNet $ replicate 10 (input, target)
        err2 = test trained [(input, target)]
    print err2
    where
        input = N.vector [1, 9, 3]
        target = N.vector [0, 0.4]

main :: IO ()
main = do
    runIris