{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Network(Network(..), forward, forwardLog) where
import Layer

data Network = Network {
    layers :: [Layer]
    } deriving (Show)

forward :: Network -> Vec -> Vec
forward net input = case layers net of
    [] -> input
    (l:ls) -> forward net {layers = ls} $ evaluate l input

logGetOutput :: [(Vec, Vec)] -> Vec
logGetOutput = snd . last

forwardLog :: Network -> Vec -> [(Vec, Vec)]
forwardLog net input = case layers net of
    [] -> [(input, input)]
    (l:ls) -> (input, calcS l input) : forwardLog net {layers = ls} (evaluate l input)
