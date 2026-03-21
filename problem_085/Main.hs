-- Problem 085: Counting Rectangles
-- Find the area of the grid with the nearest number of rectangles to 2,000,000.
-- Answer: 2772

module Main where

import Bench (runBench)

-- Number of rectangles in an m x n grid = C(m+1,2) * C(n+1,2) = m*(m+1)*n*(n+1)/4
countRects :: Int -> Int -> Int
countRects m n = m * (m + 1) * n * (n + 1) `div` 4

solve :: Int
solve = snd $ minimum
    [ (abs (countRects m n - 2000000), m * n)
    | m <- [1..100]
    , n <- [m..100]
    , countRects m n > 1000000
    , countRects m n < 3000000
    ]

main :: IO ()
main = runBench 85 (return solve)
