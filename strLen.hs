module Codewars.G964.Longestconsec where

import Data.List

longestConsec :: [String] -> Int -> String
longestConsec strarr k = let arrays = getArrays strarr k
                             c = map (concat) arrays
                             in maximumBy (\a b -> length a `compare` length b) c

getArrays :: [String] -> Int -> [[String]]
getArrays strarr k
  | length strarr <= k = [strarr]
  | otherwise          = (take k strarr):(getArrays (tail strarr) k)
