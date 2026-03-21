-- Problem 096: Su Doku
-- Solve all 50 Sudoku puzzles and sum the 3-digit numbers in the top-left corners.
-- Answer: 24702

module Main where

import Bench (runBench)
import Data.Char (digitToInt, intToDigit)
import Data.List (find, (\\))
import Data.Maybe (fromJust, isJust)

type Grid = [[Int]]

parseGrids :: String -> [Grid]
parseGrids s = go (lines s)
  where
    go [] = []
    go ls = let (grid, rest) = splitAt 10 ls
            in map (map digitToInt) (tail grid) : go rest

solve1 :: Grid -> Grid
solve1 grid = case solveSudoku grid of
    Just g  -> g
    Nothing -> grid

solveSudoku :: Grid -> Maybe Grid
solveSudoku grid
    | null empties = Just grid
    | otherwise    = find isJust attempts >>= id
  where
    empties = [(r, c) | r <- [0..8], c <- [0..8], grid !! r !! c == 0]
    (r, c) = head empties
    possible = [1..9] \\ (rowVals ++ colVals ++ boxVals)
    rowVals = grid !! r
    colVals = map (!! c) grid
    boxVals = [grid !! i !! j | i <- [br..br+2], j <- [bc..bc+2]]
    br = (r `div` 3) * 3
    bc = (c `div` 3) * 3
    attempts = [solveSudoku (setCell grid r c v) | v <- possible]
    setCell g row col val = take row g ++ [take col (g !! row) ++ [val] ++ drop (col+1) (g !! row)] ++ drop (row+1) g

cornerNum :: Grid -> Int
cornerNum g = g !! 0 !! 0 * 100 + g !! 0 !! 1 * 10 + g !! 0 !! 2

main :: IO ()
main = runBench 96 $ do
    contents <- readFile "sudoku.txt"
    let grids = parseGrids contents
        solved = map solve1 grids
    return (sum (map cornerNum solved) :: Int)
