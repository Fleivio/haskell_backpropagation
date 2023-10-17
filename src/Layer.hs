module Layer (Layer (..), LayerTrainPack (..), Vec, Delta, evaluate, calcS, calcZ, mkLayer, calcDfZ, backpropLayer, backpropLastLayer, mkPrefLayer) where

import Functions
import  qualified Numeric.LinearAlgebra as N

type Vec = N.Vector Double

type Delta = Vec

data Layer = Layer
  { activation :: Activation,
    weights :: N.Matrix Double,
    bias :: Vec
  }

instance Show Layer where
  show (Layer _ w _) =
    "\nInput: "
      ++ show (N.cols w)
      ++ "\nOutput "
      ++ show (N.rows w)
      ++ "\n"

evaluate :: Layer -> Vec -> Vec
evaluate l = calcZ l . calcS l

calcS :: Layer -> Vec -> Vec
calcS (Layer _ w b) input = (w N.#> input) + b

calcZ :: Layer -> Vec -> Vec
calcZ (Layer f1 _ _) = N.fromList . f f1 . N.toList

calcDfZ :: Layer -> Vec -> Vec
calcDfZ (Layer f1 _ _) = N.fromList . df f1 . N.toList

calcDelta :: Layer -> Layer -> Delta -> Vec -> Delta
calcDelta layer nxtLayer nxtDelta actZ = (wt N.#> nxtDelta) * dfz
  where
    wt = N.tr $ weights nxtLayer
    dfz = calcDfZ layer actZ

data LayerTrainPack = LayerTrainPack
  { nextLayer :: Layer,
    nextDelta :: Delta,
    z :: Vec,
    prevZ :: Vec
  }
  deriving (Show)

backpropLayer :: Layer -> LayerTrainPack -> Double -> (Layer, Delta)
backpropLayer layer (LayerTrainPack nxtLayer nxtDelta actZ prvZ) rate =
  ( layer
      { weights = weights layer - N.scale rate wDeriv,
        bias = bias layer - N.scale rate bDeriv
      },
    delta
  )
  where
    wDeriv = delta `N.outer` prvZ
    bDeriv = delta
    delta = calcDelta layer nxtLayer nxtDelta actZ

backpropLastLayer :: Layer -> Delta -> Vec -> Double -> Layer
backpropLastLayer layer delta prvZ rate =
  layer
    { weights = weights layer - N.scale rate wDeriv,
      bias = bias layer - N.scale rate bDeriv
    }
  where
    wDeriv = delta `N.outer` prvZ
    bDeriv = delta

mkLayer :: Activation -> Int -> Int -> IO Layer
mkLayer f1 l c = do
  w <- N.rand l c
  b <- N.rand l 1 >>= return . head . N.toColumns
  return $ Layer f1 w b

mkPrefLayer :: Activation -> Int -> Int -> Layer
mkPrefLayer f1 l c = Layer f1 w b
  where
    w = N.matrix l (replicate (l * c) 0) :: N.Matrix Double
    b = N.vector (replicate l 0)