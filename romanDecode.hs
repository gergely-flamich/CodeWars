module Roman where

import Control.Monad

vals = [('I', 1)
       ,('V', 5)
       ,('X', 10)
       ,('L', 50)
       ,('C', 100)
       ,('D', 500)
       ,('M', 1000)]

foldFn :: Char -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)
foldFn next (_, (Just 0)) = let startVal = lookup next vals in
                               (startVal, startVal)
foldFn next (prev, progress) = let val = do
                                     prevVal <- prev
                                     newVal <- lookup next vals
                                     if prevVal > newVal
                                       then return . negate $ newVal
                                       else return newVal
                               in (fmap (abs) val, liftM2 (+) val progress)

solution :: String -> Int
solution str = let (Just res) = snd (foldr foldFn (Just 0, Just 0) str) in
                 res
