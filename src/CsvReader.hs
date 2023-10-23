module CsvReader(readCSV) where

import Data.List.Split

readCSV :: String -> IO [[String]]
readCSV filename = do
  csvData <- readFile filename
  return . map (splitOn ",") . tail . lines $ csvData