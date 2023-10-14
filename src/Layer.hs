{-# HLINT ignore "Use <&>" #-}
module Layer(Layer(..), Vec, evaluate, calcS, calcZ, mkLayer) where
import qualified Numeric.LinearAlgebra as N
import Functions

type Vec = N.Vector Double

data Layer = Layer {
    activation :: Activation,
    weights :: N.Matrix Double,
    bias :: Vec
}

instance Show Layer where
    show (Layer _ w b) = "Layer " ++ show w ++ " " ++ show b


evaluate :: Layer -> Vec -> Vec
evaluate l = calcZ l . calcS l

calcS :: Layer -> Vec -> Vec
calcS (Layer _ w b) input = (w N.#> input) + b

calcZ :: Layer -> Vec -> Vec
calcZ (Layer f1 _ _) = N.fromList . f f1 . N.toList

mkLayer :: Activation -> Int -> Int -> IO Layer
mkLayer f1 n m = do
    w <- N.rand m n
    b <- N.rand m 1 >>= return . head . N.toColumns
    return $ Layer f1 w b