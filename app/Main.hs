module Main (main) where
import qualified Numeric.LinearAlgebra as N

import Layer

main :: IO ()
main = print $ evaluate testLayer (N.vector [1,2]) 
