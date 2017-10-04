module Snail where

import Data.List

snail :: [[Int]] -> [Int]
snail [] = []
snail array = let ([firstRow], rest) = splitAt 1 array in
                firstRow ++ (snail . transpose. map (reverse) $ rest)


array = [[1,2,3],
          [4,5,6],
          [7,8,9]] :: [[Int]]
