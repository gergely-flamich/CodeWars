module Codewars.Kata.FactorialTail where

import Data.List (genericLength, minimumBy)
import Data.Char (intToDigit)
import qualified Data.Map as M
import Numeric (showIntAtBase)
import Control.Monad.Writer

-- this is too slow for larger values
zeroes :: Integral a => a -> a -> a
zeroes b num = let powers = map (b^) [1..]
                   cap = last $ takeWhile (<num*num) powers
                   zeroWriter = foldl (factorFold b cap) (return 1) [2..num]
               in getSum . snd . runWriter $ zeroWriter

factorFold :: Integral a => a -> a ->  Writer (Sum a) a -> a -> Writer (Sum a) a
factorFold base cap writer current = do
  x <- writer
  getRemainder base cap (x * current)

getRemainder :: Integral a => a -> a -> a -> Writer (Sum a) a
getRemainder base cap prod
  | prod `mod` base == 0 = do
      let newProd = prod `div` base
      tell 1
      getRemainder base cap newProd
  | otherwise            = return (prod `mod` cap)
