{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Network(Network(..), forward) where

import qualified Numeric.LinearAlgebra as N
import Layer

data Network = Network {
    layers :: [Layer]
    }

forward :: Network -> N.Vector Double -> N.Vector Double
forward net input = case layers net of
    [] -> input
    (l:ls) -> forward net {layers = ls} $ evaluate l input