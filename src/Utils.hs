module Utils(sigmoid, softmax, crossEntropy, squaredMeanError) where

sigmoid :: [Double] -> [Double]
sigmoid = fmap (\x -> 1 / (exp (negate x) + 1))

softmax :: [Double] -> [Double]
softmax xs = fmap (/ sum xs) xs

crossEntropy :: [Double] -> [Double] -> Double
crossEntropy ys yhs = negate $ sum $ zipWith (*) yhs $ fmap log ys

squaredMeanError :: [Double] -> [Double] -> Double
squaredMeanError ys yhs = sum $ zipWith (\y yh -> (y - yh) ** 2) ys yhs