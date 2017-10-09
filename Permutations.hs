module Codewars.Kata.Permutations (permutations) where

import Data.List (nub)

permutations :: String -> [String]
permutations "" = [""]
permutations (x:xs) = getPerm xs [[x]]

getPerm :: String -> [String] -> [String]
getPerm "" perms = perms
getPerm (symb:symbs) perms = getPerm symbs (nub . concat . map (permFold [symb] []) $ perms)

permFold :: String -> String -> String -> [String]
permFold symb init [] = [reverse init ++ symb]
permFold symb init rest@(t:tail) = (reverse init ++ symb ++ rest):permFold symb (t:init) tail
