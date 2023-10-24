module Layer (Layer (..), Vec, Delta, evaluate, mkLayer, backpropLayer, calcDfZ, backpropLastLayer, mkPrefLayer, completeShow) where

import Functions
import  qualified Numeric.LinearAlgebra as N

type Vec = N.Vector Double
type Delta = Vec

data Layer = 
  Layer {
    activation :: Activation,
    weights :: N.Matrix Double,
    bias :: Vec,
    input :: Vec,
    output :: Vec,
    delta :: Delta
  }

completeShow :: Layer -> String
completeShow (Layer af w b i o d) =
  "\nMat: " ++ show w
    ++ "\nBias: " ++ show b
    ++ "\nActivation: " ++ show af
    ++ "\n\tInput: " ++ show i
    ++ "\n\tDelta: " ++ show d
    ++ "\n\tOutput: " ++ show o
    ++ "\n"

instance Show Layer where
  show l =
    "\nInput: "
      ++ show (N.cols $ weights l)
      ++ "\nOutput "
      ++ show (N.rows $ weights l)
      ++ "\n"

evaluate :: Layer -> Vec -> Layer
evaluate l v = 
  l {
    input = v,
    output = calcZ l $ calcS l v
  }

calcS :: Layer -> Vec -> Vec
calcS l inp = (weights l N.#> inp) + bias l

calcZ :: Layer -> Vec -> Vec
calcZ l = N.fromList . f (activation l) . N.toList

calcDfZ :: Layer -> Vec -> Vec
calcDfZ l = N.fromList . df (activation l) . N.toList

calcDelta :: Layer -> Layer -> Layer
calcDelta l ln = 
  -- trace ("dfz: " ++ show dfz ++ "\nnxt delt: " ++ show nxtDelta ++ "\n wt: " ++ show wt)
  l {
    delta = (wt N.#> nxtDelta) * dfz
  }
  where
    nxtDelta = delta ln
    actZ = output l
    wt = N.tr $ weights ln
    dfz = calcDfZ l actZ

scaleLayer :: Layer -> Double -> Layer
scaleLayer l rate = 
  l {
    weights = weights l - N.scale rate wD,
    bias = bias l - N.scale rate bD
  }
  where
    wD = delta l `N.outer` input l
    bD = delta l

backpropLayer :: Layer -> Layer -> Double -> Layer
backpropLayer l nxtLayer = scaleLayer (calcDelta l nxtLayer)

backpropLastLayer :: Layer  -> Vec -> Double -> Layer
backpropLastLayer l trg = 
  scaleLayer l {
                delta = (output l - trg) * calcDfZ l (output l)
              }


mkL :: Activation -> N.Matrix Double -> Vec -> Layer
mkL af m v =
  Layer {
    activation = af,
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
  return $ mkL f1 (scal w) (scal b)
  where
    scal x = (2*) $ x - 0.5

mkPrefLayer :: Activation -> Int -> Int -> Layer
mkPrefLayer f1 l c = mkL f1 w b
  where
    w = N.matrix c (take (l * c) staff) :: N.Matrix Double
    b = N.vector (take l staff)
    staff = cycle [-1,0.5,1]