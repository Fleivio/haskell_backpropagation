module Functions (Activation (..), ErrorFunction ,sigmoid, softmax, id_, crossEntropy, squaredMeanError) where

data Activation = Act
  { f :: [Double] -> [Double],
    df :: [Double] -> [Double]
  }

type ErrorFunction = [Double] -> [Double] -> Double

sigmoid :: Activation
sigmoid =
  Act
    { f = fmap (\x -> 1 / (1 + exp (negate x))),
      df = fmap (\x -> x * (1 - x))
    }

softmax :: Activation
softmax =
  Act
    { f = \xs -> fmap (/sum xs) xs,
      df = ([1 :: Double] <*)
    }

id_ :: Activation
id_ =
  Act
    { f = id,
      df = id
    }

crossEntropy :: ErrorFunction
crossEntropy ys yhs = negate $ sum $ zipWith (*) yhs $ fmap log ys

squaredMeanError :: ErrorFunction
squaredMeanError ys yhs = sum $ zipWith (\y yh -> (y - yh) ** 2) ys yhs
