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
    l2 <- mkLayer softmax 3 5
    l3 <- mkLayer softmax 3 3
    return $ Network [l1, l2, l3] 0.1 squaredMeanError 

main :: IO ()
main = do
    datas <- readCSV "Iris.csv"
    n2 <- irisNet
    gen <- newStdGen
    let 
        datas' = map tail datas
        labels = map last datas
        
        inputs = (map ( map read . init) datas') :: [[Double]]
        targets = map (\x -> map (tobin . (==x)) tlab) labels
            where 
                tobin b = if b then 1 else 0
                tlab = ["Iris-setosa", "Iris-versicolor", "Iris-virginica"]
        
        fullData = zip inputs targets
         
        shufData = shuffle' fullData (length fullData) gen
        trainData = map (\(x,y) -> (N.vector x, N.vector y)) $ take 140 shufData
        testData = map (\(x,y) -> (N.vector x, N.vector y)) $ drop 100 shufData

        err1 = test n2 testData
    
    putStrLn $ "Erro1 : " ++ show err1

    let
        trained = train n2 trainData
        err2 = test trained testData

    putStrLn $ "Erro1 : " ++ show err2