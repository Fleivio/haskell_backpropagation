module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer
import Network 

myNet :: Network
myNet = Network [testLayer, testLayer]  

main :: IO ()
main = do
    print $ forward myNet (N.vector [1,2])
    print $ forwardLog myNet (N.vector [1,2])
