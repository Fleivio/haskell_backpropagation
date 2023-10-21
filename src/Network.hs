{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Network (Network (..), forward, getOutput, train, test, calcErrN) where

import Layer
import Functions
import qualified Numeric.LinearAlgebra as N
import Debug.Trace (trace)

data Network = Network
  { layers :: [Layer],
    learningRate :: Double,
    errF :: ErrorFunction
  }

instance Show Network where
  show net = show (layers net)

instance Semigroup Network where
  net1 <> net2 = net1 {layers = layers net1 ++ layers net2}

getOutput :: Network -> Vec
getOutput net = output $ last $ layers net

forward :: Network -> Vec -> Network
forward net input =
  case layers net of
    [] -> net
    (l : ls) -> net { layers = [newLayer] } <> nextLayers
      where
        newLayer = evaluate l input
        nextLayers = -- trace "a" $
          forward net {layers = ls} (output newLayer)

calcErr :: Network -> Vec -> Vec -> Double
calcErr net out trg = errF net (N.toList out) (N.toList trg)

calcErrN :: Network -> [(Vec, Vec)] -> Double
calcErrN net datas = sum (map error1 datas) / fromIntegral (length datas)
  where
    error1 = uncurry $ calcErr net

test :: Network -> [(Vec, Vec)] -> Double
test net datas =
  sum (map error1 datas) / fromIntegral (length datas)
  where
    error1 (input, target)
      = let output = getOutput $ forward net input
        in --trace (show output) $
           calcErr net output target

train :: Network -> [(Vec, Vec)] -> Network
train net [] = net
train net ((input, target) : xs) = train (backprop net input target) xs

backprop :: Network -> Vec -> Vec -> Network
backprop net' input target =
  net {
    layers = newLayers
  }
  where
    net = forward net' input

    layerOut = backpropLastLayer
      (last $ layers net)
      target
      (learningRate net)

    revLayers = reverse $ init (layers net) ++ [layerOut]
    newLayers = reverse $ layerOut : backprop' revLayers

    backprop' :: [Layer] -> [Layer]
    backprop' (ln:la:ls) =
      let
        lnew = backpropLayer la ln (learningRate net)
      in lnew : backprop' (la:ls)
    backprop' [_] = []
    backprop' [] = []


{--

train :: Network -> [(Vec, Vec)] -> Network
train net [] = net
train net ((input, target) : xs) = train (backprop net input target) xs

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
    errorOut = output - target
    deltaOut = errorOut * (calcDfZ $ last $ layers net) output

    layerOut =
      backpropLastLayer
        (last $ layers net)
        deltaOut
        (last $ init forwardLog')
        (learningRate net)

    backprop' :: [(Vec, Vec, Layer, Layer)] -> Delta -> [Layer]
    backprop' [] _ = []
    backprop' ltps nxtDelta = --trace (show nxtDelta ++ show delta ++ "\n") $ 
      backprop' (init ltps) delta ++ [al]
      where
        (actZ, pz, l, nl) = last ltps
        tp = LayerTrainPack nl nxtDelta actZ pz
        (al, delta) = backpropLayer l tp (learningRate net)
--}