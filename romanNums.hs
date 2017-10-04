import Control.Monad

digitVals = [('1', [(0, "I")
                   ,(1, "X")
                   ,(2, "C")
                   ,(3, "M")]),
             ('2', [(0, "II")
                   ,(1, "XX")
                   ,(2, "CC")
                   ,(3, "MM")]),
             ('3', [(0, "III")
                   ,(1, "XXX")
                   ,(2, "CCC")
                   ,(3, "MMM")]),
             ('4', [(0, "IV")
                   ,(1, "XL")
                   ,(2, "CD")]),
             ('5', [(0, "V")
                   ,(1, "L")
                   ,(2, "D")]),
             ('6', [(0, "VI")
                   ,(1, "LX")
                   ,(2, "DC")]),
             ('7', [(0, "VII")
                   ,(1, "LXX")
                   ,(2, "DCC")]),
             ('8', [(0, "VIII")
                   ,(1, "LXXX")
                   ,(2, "DCCC")]),
             ('9', [(0, "IX")
                   ,(1, "XC")
                   ,(2, "CM")])]

lookupDigit :: Char -> Int -> String
lookupDigit digit position = let romanNum = do
                                 posList <- lookup digit digitVals
                                 lookup position posList
                             in
                               case romanNum of
                                 Nothing -> ""
                                 Just str -> str

solution :: Integer -> String
solution num = let numStr = show num
                   numLen = length numStr - 1
                   posList = reverse [0 .. numLen]
               in join $ zipWith (lookupDigit) numStr posList
