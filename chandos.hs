import Numeric (showIntAtBase)
import Data.Char (intToDigit)

chandos :: Int -> Int
chandos n =
  let binStr = showIntAtBase 2 intToDigit n "" in
  foldl foldFn 0 binStr

foldFn :: Int -> Char -> Int
foldFn acc c
  | c == '0' = acc * 5
  | c == '1' = (acc + 1) * 5

chandos' :: Int -> Int
chandos' 0 = 0
chandos' n
  | n `mod` 2 == 1 = 5 * (1 + (chandos' $ (n - 1) `div` 2))
  | otherwise  = 5 * (chandos' $ n `div` 2)
