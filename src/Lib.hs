module Lib
    ( someFunc
    ) where

import qualified Numeric.LinearAlgebra as N

someFunc :: IO ()
someFunc =
    print $ (N.ident 3 :: N.Matrix Double)
