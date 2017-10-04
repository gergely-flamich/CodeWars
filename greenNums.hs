import Data.List


squareMatches :: (Integral a, Show a) => a -> Bool
squareMatches n = let numStr = show n
                      subtracted = n * n - n
                      lastDigits = take (length numStr) . reverse . show $ subtracted
                  in all (=='0') lastDigits

green :: Int -> Integer
green 1 = 1
green 2 = 5
green 3 = 6
green n = checkSquares (n - 2, 5, 6)

data FiveOrSix a = Five a | Six a deriving (Show, Read, Eq)

findFiveOrSix :: (Show a, Integral a) => a -> Int -> (a, a) -> FiveOrSix a
findFiveOrSix base exponent (five, six)
  | squareMatches candFive = Five candFive
  | squareMatches candSix  = Six candSix
  | otherwise              = findFiveOrSix (base + 1) exponent (five, six)
  where startMagnitude = base * 10 ^ exponent
        candFive = startMagnitude + five
        candSix = startMagnitude + six

checkSquares :: (Integral a, Show a) => (Int, a, a) -> a
checkSquares (0, fives, sixes) = max fives sixes
checkSquares (n, fives, sixes) = let greaterLength = length (show (max fives sixes))
                                     nextMatch = case findFiveOrSix 1 (greaterLength - 1) (fives, sixes) of
                                                   Five x -> (n - 1, x, sixes)
                                                   Six x  -> (n - 1, fives, x)
                                 in checkSquares nextMatch

