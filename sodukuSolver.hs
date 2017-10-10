module Sudoku where

import Data.List


type Position = (Int, Int)
type Board = [[Int]]

sudoku :: Board -> Board
sudoku board = let unfilled = getAllUnfilled board
               in case fillAll board unfilled of
                    Nothing -> error "Insolvable"
                    Just b -> b

getAllUnfilled :: Board -> [(Int, Int)]
getAllUnfilled board = let unfilledRowPositions = map (getZerosInRow) board
                           withRowNums = zip unfilledRowPositions [1..9]
                           positions = concat $ map (\(nums, row) -> zip (repeat row) nums) withRowNums
                       in positions
  where getZerosInRow line = map (snd) $ filter (\x -> fst x == 0) (zip line [1..9])

fill :: Board -> Position -> Int -> Board
fill board (row, col) num = let (top, (focusRow:bottom)) = splitAt (row-1) board
                                (pre, (delItem:rest)) = splitAt (col-1) focusRow
                                newRow = pre ++ (num:rest)
                                newBoard = top ++ (newRow:bottom)
                            in newBoard

fillAll :: Board -> [Position] -> Maybe Board
fillAll board [] = Just board
fillAll board (pos:positions) = case getCandidates board pos of
                               Nothing -> Nothing
                               Just cands -> tryCands cands
  where tryCands [] = Nothing
        tryCands (cand:cands) = let newBoard = fill board pos cand
                                in case fillAll newBoard positions of
                                     Nothing -> tryCands cands
                                     Just b -> Just b

getCandidates :: Board -> Position -> Maybe [Int]
getCandidates board pos = case filter (allowed board pos) [1..9] of
                            [] -> Nothing
                            nums -> Just nums

allowed :: Board -> Position -> Int -> Bool
allowed board pos@(row, col) candidate = let rowCheck = allowedInList (board !! (row - 1)) candidate
                                             colCheck = allowedInList (map (!! (col - 1)) board) candidate
                                             subSquareCheck = allowedInList (getSubSquare board pos) candidate
                                         in rowCheck && colCheck && subSquareCheck

getSubSquare :: Board -> Position -> [Int]
getSubSquare board (r, c) = let row = r - 1
                                col = c - 1
                                startRow = row - (row `mod` 3)
                                startCol = col - (col `mod` 3)
                                reducedRows = cutThreeAt startRow board
                                subSquare = concat $ map (cutThreeAt startCol) reducedRows
                            in subSquare
  where cutThreeAt start l = take 3 . snd $ splitAt start l

allowedInList :: [Int] -> Int -> Bool
allowedInList row x = all (/=x) row

puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]] :: [[Int]]
