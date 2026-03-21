-- Problem 086: Cuboid Route
-- Find the least M such that the number of cuboid solutions exceeds one million.
-- Answer: 1818

module Main where

import Bench (runBench)

isSquare :: Int -> Bool
isSquare n = let s = floor (sqrt (fromIntegral n :: Double)) in s * s == n

countSolutions :: Int -> Int
countSolutions m = sum
    [ if a == b then bc `div` 2 else bc
    | bc <- [2..2*m]
    , let s = m * m + bc * bc
    , isSquare s
    , let a = min m bc
    , let b = max 1 (bc - m)
    , a >= b
    , let count = (a - b) `div` 1 + if bc <= m then bc `div` 2 else a - (bc - a)
    ]

-- Simpler approach
solve :: Int
solve = head [m | m <- [1..], totalSolutions m > 1000000]
  where
    totalSolutions m = sum [solutions m bc | bc <- [2..2*m]]
    solutions m bc
        | isSquare (m*m + bc*bc) = if bc <= m then bc `div` 2 else m - (bc - 1) `div` 2
        | otherwise = 0

main :: IO ()
main = runBench 86 (return solve)
