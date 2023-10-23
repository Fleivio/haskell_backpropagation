module Main (main) where

import CsvReader
import Functions
import Layer
import Network
import qualified Numeric.LinearAlgebra as N
import System.Random
import System.Random.Shuffle

irisNet :: IO Network
irisNet = do
  l1 <- mkLayer sigmoid 8 4
  l2 <- mkLayer softmax 3 8
  return $ Network [l1, l2] 0.01 crossEntropy

runIris :: IO ()
runIris = do
  datas <- readCSV "Iris.csv"
  gen <- newStdGen
  n2 <- irisNet

  let 
		datas' = map (init . tail) datas
		labels = map last datas

		inputs = map (map read) datas' :: [[Double]]
		targets = map (toOneHot tlab) labels
			where
				tlab = ["Iris-setosa", "Iris-versicolor", "Iris-virginica"]

		fullData = zip inputs targets

		shufData = shuffle' fullData (length fullData) gen
		trainData = map (\(x, y) -> (N.vector x, N.vector y)) $ take 140 shufData
		testData = map (\(x, y) -> (N.vector x, N.vector y)) $ drop 140 shufData

		err1 = test n2 testData
		trained = train n2 $ concat (replicate 200 trainData)
		err2 = test trained testData

  putStrLn $ "Erro1 : " ++ show err1
  putStrLn $ "Erro2 : " ++ show err2

main :: IO ()
main = do
  runIris