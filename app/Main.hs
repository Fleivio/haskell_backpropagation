{-# HLINT ignore "Use <$>" #-}
module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer (mkLayer)
import Functions
import Network ( forward, Network(Network), forwardLog ) 

mkMyNet :: IO Network
mkMyNet = do
    l1 <- mkLayer sigmoid 2 3
    l2 <- mkLayer sigmoid 3 5
    l3 <- mkLayer sigmoid 5 3
    l4 <- mkLayer softmax 3 3
    return $ Network [l1, l2, l3, l4]

main :: IO ()
main = do
    myNet <- mkMyNet 
    print $ forward myNet (N.vector [1,2])
    print $ forwardLog myNet (N.vector [1,2])
