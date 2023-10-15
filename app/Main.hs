module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer
import Functions
import Network
import DataGenerator

mkMyNet :: IO Network
mkMyNet = do
    l1 <- mkLayer sigmoid 4 2
    l2 <- mkLayer sigmoid 2 4
    l3 <- mkLayer sigmoid 4 2
    l4 <- mkLayer sigmoid 2 4
    l5 <- mkLayer sigmoid 2 2
    return $ Network [l1, l2, l3, l4, l5] 0.01

singleTrain :: IO ()
singleTrain = do
    let 
        input = N.vector [1, 2]
        target = N.vector [2, 4]
    myNet <- mkMyNet

    let 
        error1 = test myNet squaredMeanError [(input, target)]
    putStrLn $ "Error: " ++ show error1

    let
        trainedNet = backprop myNet input target
    
    let 
        error2 = test trainedNet squaredMeanError [(input, target)]
    putStrLn $ "Error: " ++ show error2



main :: IO ()
main = do
    let 
        size = 50000
    
    datas <- genData 2 size
    myNet <- mkMyNet
    
    let
        training = take 40000 datas
        testing = drop 10000 datas

    let 
        error1 = test myNet squaredMeanError testing
    putStrLn $ "Error: " ++ show error1


    putStrLn "Training..."
    let
        trainedNet = train myNet training

    putStrLn "Testing..."
    let
        error2 = test trainedNet squaredMeanError testing
    putStrLn $ "Error: " ++ show error2
