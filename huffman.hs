module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Monad

data Bit = Z | O deriving (Eq, Show)
data HuffTree a = Empty | Node ([a], Int) (HuffTree a) (HuffTree a) deriving (Eq, Show)

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies symbols = M.assocs (foldl (\acc x -> M.insertWith' (+) x 1 acc) M.empty symbols)

singleton :: (a, Int) -> HuffTree a
singleton (symbol, freq) = Node ([symbol], freq) Empty Empty

getNodeFreq :: HuffTree a -> Int
getNodeFreq (Node (_, f) _ _) = f

buildTreeFromSaplings :: [HuffTree a] -> HuffTree a
buildTreeFromSaplings [] = Empty
buildTreeFromSaplings [x] = x
buildTreeFromSaplings nodes@(_:_:xs) =
  let newList = sortOn getNodeFreq nodes
      ([n1, n2], rest) = splitAt 2 newList
      (Node (v1, f1) l1 r1) = n1
      (Node (v2, f2) l2 r2) = n2
      newNode = Node (v1 ++ v2, f1 + f2)
                     (Node (v1, f1) l1 r1)
                     (Node (v2, f2) l2 r2)
  in  buildTreeFromSaplings (newNode:rest)

flattenTreeToCodes :: HuffTree a -> [(a, [Bit])]
flattenTreeToCodes Empty = []
flattenTreeToCodes (Node (symbol, _) l r)
  | length symbol == 1 = [(symbol !! 0, [])]
  | otherwise          = let leftList = map (\(s, p) -> (s, Z:p)) (flattenTreeToCodes l)
                             rightList = map (\(s, p) -> (s, O:p)) (flattenTreeToCodes r)
                         in leftList ++ rightList

encStr :: (Eq a) => [(a, [Bit])] -> [a] -> Maybe [Bit]
encStr _ [] = Just []
encStr codingTable symbols = do
  let ([s], ss) = splitAt 1 symbols
  code <- lookup s codingTable
  rest <- encStr codingTable ss
  return (code ++ rest)


decBitStr :: HuffTree a -> [Bit] -> Maybe [a]
decBitStr _ [] = Just []
decBitStr codeTree bitString = let symbolList = snd $ foldl (decFold codeTree) (codeTree, []) bitString in
                                 sequence . reverse $ symbolList

decFold :: HuffTree a -> (HuffTree a, [Maybe a]) -> Bit -> (HuffTree a, [Maybe a])
decFold codeTree (progress, decoded) bit = case traverseCodeTree progress bit of
                                             Empty -> (progress, [Nothing])
                                             tree@(Node (s, _) _ _) -> if length s == 1
                                                              then (codeTree, (Just $ s !! 0):decoded)
                                                              else (tree, decoded)

traverseCodeTree :: HuffTree a -> Bit -> HuffTree a
traverseCodeTree Empty _ = Empty
traverseCodeTree (Node (s, _) l r) bit
  | length s == 1 = Empty
  | bit == Z      = l
  | bit == O      = r

encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode [] _ = Nothing
encode [x] _ = Nothing
encode freqs string = encStr (flattenTreeToCodes . buildTreeFromSaplings . map (singleton) $ freqs) string

decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode [] _ = Nothing
decode [x] _ = Nothing
decode freqs bitString = decBitStr (buildTreeFromSaplings . map (singleton) $ freqs) bitString

--TESTS
fs = frequencies "aaaabcc"
res = encode fs "aaaabcc"
dec = decode fs [O,O,O,O,Z,Z,Z,O,Z,O]
testSeq = [O,O,O,O,Z,Z,Z,O,Z,O]

testTree = buildTreeFromSaplings . map (singleton) $ fs
