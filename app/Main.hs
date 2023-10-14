{-# HLINT ignore "Use <$>" #-}
module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer (mkLayer)
import Functions
import Network ( forward, Network(Network), forwardLog ) 

mkMyNet :: IO Network
mkMyNet = do
    l1 <- mkLayer sigmoid 2 3
    l2 <- mkLayer sigmoid 3 2  
    return $ Network [l1, l2]

main :: IO ()
main = do
    myNet <- mkMyNet 
    print $ forward myNet (N.vector [1,2])
    print $ forwardLog myNet (N.vector [1,2])
