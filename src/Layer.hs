module Layer (Layer (..), Vec, Delta, evaluate, mkLayer, backpropLayer, calcDfZ, backpropLastLayer, mkPrefLayer, completeShow) where

import Functions
import  qualified Numeric.LinearAlgebra as N
import Debug.Trace

type Vec = N.Vector Double
type Delta = Vec

data Layer = Layer
  { activation :: Activation,
    weights :: N.Matrix Double,
    bias :: Vec,
    input :: Vec,
    output :: Vec,
    delta :: Delta
  }

completeShow :: Layer -> String
completeShow l =
  "\nInput: "
    ++ show (N.cols $ weights l)
    ++ "\nOutput "
    ++ show (N.rows $ weights l)
    ++ "\nWeights: "
    ++ show (weights l)
    ++ "\nBias: "
    ++ show (bias l)
    ++ "\n"

instance Show Layer where
  show l =
    "\nInput: "
      ++ show (N.cols $ weights l)
      ++ "\nOutput "
      ++ show (N.rows $ weights l)
      ++ "\n"

evaluate :: Layer -> Vec -> Layer
evaluate l v = l {
    input = v,
    output = calcZ l $ calcS l v
  }

calcS :: Layer -> Vec -> Vec
calcS l input = (weights l N.#> input) + bias l

calcZ :: Layer -> Vec -> Vec
calcZ l = N.fromList . f (activation l) . N.toList

calcDfZ :: Layer -> Vec -> Vec
calcDfZ l = N.fromList . df (activation l) . N.toList

calcDelta :: Layer -> Layer -> Layer
calcDelta l ln = --trace "calcDelta"
  l {
    delta = (wt N.#> nxtDelta) * dfz
  }
  where
    nxtDelta = delta ln
    actZ = output l
    wt = N.tr $ weights ln
    dfz = calcDfZ l actZ

backpropLayer :: Layer -> Layer -> Double -> Layer
backpropLayer l' nxtLayer rate = -- trace "backpropLayer"
  l {
      weights = weights l - N.scale rate wDeriv,
      bias = bias l - N.scale rate bDeriv
    }
  where
    l = calcDelta l' nxtLayer
    prvZ = input l
    wDeriv = delta l `N.outer` prvZ
    bDeriv = delta l

backpropLastLayer :: Layer -> Vec -> Double -> Layer
backpropLastLayer l' target rate = -- trace "backpropLastLayer"
  l {
      weights = weights l - N.scale rate wDeriv,
      bias = bias l - N.scale rate bDeriv
    }
  where
    l = l' {
        delta = (output l' - target) * calcDfZ l' (output l')
      }
    wDeriv = delta l `N.outer` input l
    bDeriv = delta l


mkL :: Activation -> N.Matrix Double -> Vec -> Layer
mkL f m v =
  Layer {
    activation = f,
    weights = m,
    bias = v,
    input = N.vector (replicate c 1),
    output = N.vector (replicate l 1),
    delta = N.vector (replicate l 1)
  }
  where
    l = N.rows m
    c = N.cols m

mkLayer :: Activation -> Int -> Int -> IO Layer
mkLayer f1 l c = do
  w <- N.rand l c
  b <- N.rand l 1 >>= return . head . N.toColumns
  return $ mkL f1 w b

mkPrefLayer :: Activation -> Int -> Int -> Layer
mkPrefLayer f1 l c = mkL f1 w b
  where
    w = N.matrix c (replicate (l * c) 1) :: N.Matrix Double
    b = N.vector (replicate l 1)