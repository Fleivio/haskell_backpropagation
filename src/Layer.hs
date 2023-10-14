module Layer(Layer(..), evaluate, isLayer3, testLayer) where
import qualified Numeric.LinearAlgebra as N

data Layer = Layer {
    activation :: [Double] -> [Double],
    weights :: N.Matrix Double,
    bias :: N.Vector Double
}

instance Show Layer where
    show (Layer _ w b) = "Layer " ++ show w ++ " " ++ show b

instance Eq Layer where
    (==) (Layer _ w1 b1) (Layer _ w2 b2) = w1 == w2 && b1 == b2

evaluate :: Layer -> N.Vector Double -> N.Vector Double
evaluate (Layer f w b) input = N.fromList . f . N.toList $ (w N.#> input) + b

isLayer3 :: Layer
isLayer3 = Layer id (N.ident 3) (N.vector [0,0,0])

testLayer :: Layer
testLayer = Layer (fmap (*2)) (N.matrix 2 [1,2,3,4]) (N.vector [1,2])