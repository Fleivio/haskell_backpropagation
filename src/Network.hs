{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Network (Network (..), forward, getOutput, train, test) where

import Layer
import Functions
import qualified Numeric.LinearAlgebra as N

data Network = 
  Network {
    layers :: [Layer],
    learningRate :: Double,
    errF :: ErrorFunction
  }

instance Show Network where
  show net = unlines (map completeShow (layers net))
              ++ replicate 40 '-'

instance Semigroup Network where
  net1 <> net2 = net1 {layers = layers net1 ++ layers net2}

getOutput :: Network -> Vec
getOutput net = output $ last $ layers net

forward :: Network -> Vec -> Network
forward net inp =
  case layers net of
    [] -> net
    (l : ls) -> net { layers = [newLayer] } <> nextLayers
      where
        newLayer = evaluate l inp
        nextLayers =
          forward net {layers = ls} (output newLayer)

calcErr :: Network -> Vec -> Vec -> Double
calcErr net out trg = errF net (N.toList out) (N.toList trg)

test :: Network -> [(Vec, Vec)] -> Double
test net datas =
  sum (map error1 datas) / fromIntegral (length datas)
  where
    error1 (inp, trg) =
      let 
        otp = getOutput $ forward net inp
      in
        calcErr net otp trg

train :: Network -> [(Vec, Vec)] -> Network
train net [] = net
train net ((inp, trg) : xs) = --trace (show trained) $
                              train trained xs
  where
    trained = backprop net inp trg

backprop :: Network -> Vec -> Vec -> Network
backprop net' inp trg =
  net {
    layers = newLayers
  }
  where
    net = forward net' inp

    layerOut = backpropLastLayer
      (last $ layers net)
      trg
      (learningRate net)

    revLayers = reverse $ init (layers net) ++ [layerOut]
    newLayers = reverse $ layerOut : backprop' revLayers

    backprop' :: [Layer] -> [Layer]
    backprop' (ln:la:ls) =
      let
        lnew = backpropLayer la ln (learningRate net)
      in 
        lnew : backprop' (la:ls)
    backprop' [_] = []
    backprop' [] = []
