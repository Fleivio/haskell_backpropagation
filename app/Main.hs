{-# HLINT ignore "Use <$>" #-}
module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer
import Functions
import Network ( forward, Network(Network), forwardLog ) 

-- mkMyNet :: IO Network
-- mkMyNet = do
--     l1 <- mkLayer sigmoid 2 3
--     l2 <- mkLayer sigmoid 3 5
--     l3 <- mkLayer sigmoid 5 3
--     l4 <- mkLayer softmax 3 3
--     return $ Network [l1, l2, l3, l4] 0.1

-- main :: IO ()
-- main = do
--     myNet <- mkMyNet 
--     print $ forward myNet (N.vector [1,2])
--     print $ forwardLog myNet (N.vector [1,2])

-- backpropLayer :: Layer -> Layer -> Vec -> Vec -> Vec -> Double -> Layer
-- backpropLayer layer nextLayer nextDelta z prevZ rate


main :: IO ()
main = do
    let zant = N.vector [1,2,4] 
    l1 <- mkLayer sigmoid 3 3
    lnext <- mkLayer sigmoid 3 3
    print l1
    print lnext
    let l2 = backpropLayer
             l1 lnext (N.vector [1,2,0]) (N.vector [1,2,3]) zant 0.1
    print l2