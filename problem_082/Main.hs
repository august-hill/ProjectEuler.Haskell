-- Problem 082: Path Sum: Three Ways
-- Find the minimum path sum in an 80x80 matrix (up, down, and right).
-- Answer: 260324

module Main where

import Bench (runBench)

parseMatrix :: String -> [[Int]]
parseMatrix = map (map read . words . map (\c -> if c == ',' then ' ' else c)) . lines

solve :: [[Int]] -> Int
solve matrix = minimum lastCol
  where
    rows = length matrix
    cols = length (head matrix)
    firstCol = [matrix !! i !! 0 | i <- [0..rows-1]]
    lastCol = foldl processCol firstCol [1..cols-1]
    processCol prev j =
        let col = [matrix !! i !! j | i <- [0..rows-1]]
            -- Start with coming from left
            fromLeft = zipWith (+) col prev
            -- Scan down
            fromUp = scanl1 (\acc (fl, c) -> min fl (acc + c)) (zip fromLeft col)
            -- Scan up
            fromDown = scanr1 (\(fl, c) acc -> min fl (acc + c)) (zip fromLeft col)
            -- Take minimum
        in zipWith min fromUp fromDown

main :: IO ()
main = runBench 82 $ do
    contents <- readFile "matrix.txt"
    return (solve (parseMatrix contents))
