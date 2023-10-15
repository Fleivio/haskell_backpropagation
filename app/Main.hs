module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer
import Functions
import Network

mkMyNet :: IO Network
mkMyNet = do
    l1 <- mkLayer sigmoid 4 3
    l2 <- mkLayer sigmoid 3 4
    return $ Network [l1, l2] 0.1

main :: IO ()
main = do
    myNet <- mkMyNet 
    let newNet = backprop myNet (N.vector [1,2,3]) (N.vector [1,4,3])
    print newNet