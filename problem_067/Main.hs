-- Problem 067: Maximum Path Sum II
-- Find the maximum total from top to bottom in a large triangle (file-based).
-- Answer: 7273

module Main where

import Bench (runBench)

-- The triangle data is read from a file
parseTriangle :: String -> [[Int]]
parseTriangle = map (map read . words) . lines

solve :: [[Int]] -> Int
solve = head . foldr1 collapse
  where
    collapse row below = zipWith (+) row (zipWith max below (tail below))

main :: IO ()
main = runBench 67 $ do
    contents <- readFile "triangle.txt"
    let triangle = parseTriangle contents
    return (solve triangle)
