module Functions (Activation (..), ErrorFunction ,sigmoid, softmax, id_, crossEntropy, squaredMeanError, toOneHot) where

data Activation = 
  Act{ 
    fname :: String,
    f :: [Double] -> [Double],
    df :: [Double] -> [Double]
  }

instance Show Activation where
  show = fname

type ErrorFunction = [Double] -> [Double] -> Double

sigmoid :: Activation
sigmoid =
  Act {
    fname = "sigmoid",
    f = fmap sig,
    df = fmap (\x -> x * (1 - x))
  }
  where
    sig x = 1 / (1 + exp (negate x))

softmax :: Activation
softmax =
  Act {
    fname = "softmax",
    f = \xs -> fmap (`soft` xs) xs,
    df = ([1 :: Double] <*)
  }
  where
    soft x xs = exp x / sum (map exp xs)

id_ :: Activation
id_ =
  Act{
    fname = "id",
    f = id,
    df = id
  }

crossEntropy :: ErrorFunction
crossEntropy ys yhs = negate $ sum $ zipWith (*) yhs $ fmap log ys

squaredMeanError :: ErrorFunction
squaredMeanError ys yhs = sum $ zipWith (\y yh -> (y - yh) ** 2) ys yhs

toOneHot :: Eq a => [a] -> a -> [Double]
toOneHot xs x = map (tobin . (== x)) xs
  where
    tobin b = if b then 1 else 0