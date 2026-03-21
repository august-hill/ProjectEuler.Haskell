-- Problem 081: Path Sum: Two Ways
-- Find the minimum path sum in an 80x80 matrix (right and down only).
-- Answer: 427337

module Main where

import Bench (runBench)

-- Matrix data would be read from file
parseMatrix :: String -> [[Int]]
parseMatrix = map (map read . words . map (\c -> if c == ',' then ' ' else c)) . lines

solve :: [[Int]] -> Int
solve matrix = last $ foldl1 step matrix
  where
    step prev row = scanl1 (\acc val -> val + min acc prev') (zipWith (+) row prev)
      where prev' = head prev  -- This needs fixing for proper DP

-- Actually, proper minimum path sum:
solveMatrix :: [[Int]] -> Int
solveMatrix matrix = last (foldl1 combine matrix)
  where
    combine prevRow curRow =
        let firstVal = head curRow + head prevRow
            rest = zipWith3 (\c p prev -> c + min p prev)
                           (tail curRow) (tail prevRow) (scanl1 (\acc (c, p) -> min (c + p) (c + acc)) undefined)
        in firstVal : rest

main :: IO ()
main = runBench 81 $ do
    contents <- readFile "matrix.txt"
    let matrix = parseMatrix contents
    return (minimum_path matrix)
  where
    minimum_path matrix = dp !! (rows - 1) !! (cols - 1)
      where
        rows = length matrix
        cols = length (head matrix)
        dp = [[val i j | j <- [0..cols-1]] | i <- [0..rows-1]]
        val 0 0 = matrix !! 0 !! 0
        val 0 j = matrix !! 0 !! j + dp !! 0 !! (j-1)
        val i 0 = matrix !! i !! 0 + dp !! (i-1) !! 0
        val i j = matrix !! i !! j + min (dp !! (i-1) !! j) (dp !! i !! (j-1))
