module Layer(Layer(..), Vec, evaluate, isLayer3, testLayer, calcS, calcZ) where
import qualified Numeric.LinearAlgebra as N

type Vec = N.Vector Double

data Layer = Layer {
    activation :: [Double] -> [Double],
    weights :: N.Matrix Double,
    bias :: Vec
}

instance Show Layer where
    show (Layer _ w b) = "Layer " ++ show w ++ " " ++ show b

instance Eq Layer where
    (==) (Layer _ w1 b1) (Layer _ w2 b2) = w1 == w2 && b1 == b2

evaluate :: Layer -> Vec -> Vec
evaluate l = calcZ l . calcS l

calcS :: Layer -> Vec -> Vec
calcS (Layer _ w b) input = (w N.#> input) + b

calcZ :: Layer -> Vec -> Vec
calcZ (Layer f _ _) = N.fromList . f . N.toList

isLayer3 :: Layer
isLayer3 = Layer id (N.ident 3) (N.vector [0,0,0])

testLayer :: Layer
testLayer = Layer (fmap (*2)) (N.matrix 2 [1,2,3,4]) (N.vector [1,2])