module Network (Network (..), forward, forwardLog, backprop) where

import GHC.OldList (zip4)
import Layer

data Network = Network
  { layers :: [Layer],
    learningRate :: Double
  }
  deriving (Show)

forward :: Network -> Vec -> Vec
forward net input =
  case layers net of
    [] -> input
    (l : ls) -> forward net {layers = ls} $ evaluate l input

logGetOutput :: [Vec] -> Vec
logGetOutput = last

forwardLog :: Network -> Vec -> [Vec]
forwardLog net input =
  case layers net of
    [] -> [input]
    (l : ls) -> input : forwardLog net {layers = ls} (evaluate l input)

backprop :: Network -> Vec -> Vec -> Network
backprop net input target =
  net
    { layers = backprop' zPairs deltaOut ++ [layerOut]
    }
  where
    forwardLog' = forwardLog net input
    output = logGetOutput forwardLog'

    zPairs =
      zip4
        (tail forwardLog') -- z
        (init forwardLog') -- prevZ
        (layers net) -- layer
        (tail $ layers net) -- nextLayer
    errorOut = target - output
    deltaOut = errorOut * (calcDfZ $ last $ layers net) output

    layerOut =
      backpropLastLayer
        (last $ layers net)
        deltaOut
        (last $ init forwardLog')
        (learningRate net)

    backprop' :: [(Vec, Vec, Layer, Layer)] -> Delta -> [Layer]
    backprop' [] _ = []
    backprop' ltps nxtDelta = backprop' (init ltps) delta ++ [al]
      where
        (actZ, pz, l, nl) = last ltps
        tp = LayerTrainPack nl nxtDelta actZ pz
        (al, delta) = backpropLayer l tp (learningRate net)