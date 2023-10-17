{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module DataGenerator(genData) where

import qualified Numeric.LinearAlgebra as N

inputData :: Int -> Int -> IO [N.Vector Double]
inputData size quant = sequence [N.rand size 1 >>= return . head . N.toColumns 
                                | _ <- [1..quant]]

genOutPutData :: [N.Vector Double] -> (N.Vector Double -> N.Vector Double)
                 -> [N.Vector Double]
genOutPutData input rule = map rule input

genData :: Int -> Int -> IO [(N.Vector Double, N.Vector Double)]
genData size quant = do 
    ins <- inputData size quant
    let
        outs = genOutPutData ins (* vec)
    return $ zip ins outs
    where 
        vec = N.vector (take size (map (\x -> if even x then 1 else 0) [1..]))