module Functions(Activation(..), sigmoid, softmax, id_, crossEntropy, squaredMeanError) where

data Activation = Act {
    f :: [Double] -> [Double],
    df :: [Double] -> [Double]
    }

sigmoid :: Activation
sigmoid = Act {
    f = fmap (\x -> 1 / (exp (negate x) + 1)),
    df = fmap (\x -> x * (1 - x))
    }

softmax :: Activation
softmax = Act {
    f = \xs -> fmap (/ sum xs) xs,
    -- when using cross entropy, the derivative of softmax is not needed
    df = ([1 :: Double] *>) 
    }

id_ :: Activation
id_ = Act {
    f = id,
    df = id
    }

crossEntropy :: [Double] -> [Double] -> Double
crossEntropy ys yhs = negate $ sum $ zipWith (*) yhs $ fmap log ys

squaredMeanError :: [Double] -> [Double] -> Double
squaredMeanError ys yhs = sum $ zipWith (\y yh -> (y - yh) ** 2) ys yhs
