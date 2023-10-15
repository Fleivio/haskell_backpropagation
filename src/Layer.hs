{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Layer(Layer(..), Vec, evaluate, calcS, calcZ, mkLayer, calcDfZ, backpropLayer) where
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

calcDfZ :: Layer -> Vec -> Vec
calcDfZ (Layer f1 _ _) = N.fromList . df f1 . N.toList

calcDelta :: Layer -> Layer -> Vec -> Vec -> Vec
calcDelta layer nextLayer nextDelta z = (wt N.#> nextDelta) * dfz
    where
        wt = N.tr $ weights nextLayer
        dfz = calcDfZ layer z

backpropLayer :: Layer -> Layer -> Vec -> Vec -> Vec -> Double -> Layer
backpropLayer layer nextLayer nextDelta z prevZ rate
    = layer {
        weights = weights layer - N.scale rate wDeriv,
        bias    = bias layer    - N.scale rate bDeriv
    }
    where
        wDeriv = delta `N.outer` prevZ
        bDeriv = delta
        delta = calcDelta layer nextLayer nextDelta z

mkLayer :: Activation -> Int -> Int -> IO Layer
mkLayer f1 l c = do
    w <- N.rand l c
    b <- N.rand l 1 >>= return . head . N.toColumns
    return $ Layer f1 w b