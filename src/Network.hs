{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Network(Network(..), forward, forwardLog) where
import Layer
import Functions

data Network = Network {
    layers :: [Layer],
    learningRate :: Double
    } deriving (Show)

forward :: Network -> Vec -> Vec
forward net input = case layers net of
    [] -> input
    (l:ls) -> forward net {layers = ls} $ evaluate l input

logGetOutput :: [Vec] -> Vec
logGetOutput = last

forwardLog :: Network -> Vec -> [Vec]
forwardLog net input = case layers net of
    [] -> [input]
    (l:ls) -> input : forwardLog net {layers = ls} (evaluate l input)

-- backprop :: Network -> Vec -> Vec -> Network
-- backprop net input target = 
--     let
--         forwardLog' = forwardLog net input
--         output = logGetOutput forwardLog'
--         error = target - output 
--         deltaOut = error * (calcDfZ $ last $ layers net) output
--         deltas = zipWith (\(s, z) l -> calcDelta l s z) forwardLog' (reverse $ layers net)
--     in 
--         undefined