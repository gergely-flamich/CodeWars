module Codewars.Kata.FactorialTail where

import Data.List (genericLength, minimumBy)
import Data.Char (intToDigit)
import qualified Data.Map as M
import Numeric (showIntAtBase)

-- this is too slow for larger values
zeroes :: Integral a => a -> a -> a
zeroes b num = let factors = map (fst) (factorCount b 2)
                   terms = [2..num]
                   res = foldl (factorFold terms) [] factors
               in minimum res

factorFold :: Integral a => [a] -> [a] -> a -> [a]
factorFold terms factors current =
  let powers = map (current ^) [1..]
      factorTotal = foldl (\acc n -> genericLength (takeWhile (\x -> n `mod` x == 0) powers) + acc) 0 terms
  in factorTotal:factors


factorCount :: Integral a => a -> a -> [(a, a)]
factorCount 1 _ = []
factorCount n curr
  | n `mod` curr == 0 = let count = genericLength (takeWhile (\x -> n `mod` x == 0) powers)
                        in (curr, count):factorCount (n `div` (curr ^ count)) (curr + 1)
  | otherwise         = factorCount n (curr + 1)
  where
    powers = map (curr ^) [1..]
