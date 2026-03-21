-- Problem 094: Almost Equilateral Triangles
-- Find the sum of perimeters of all almost equilateral triangles with integral area
-- and perimeter <= 10^9.
-- Answer: 518408346

module Main where

import Bench (runBench)

-- Almost equilateral: sides a, a, a+1 or a, a, a-1
-- Area must be integral. Use Heron's formula.
-- For sides a, a, a+1: s = (3a+1)/2, area^2 = s(s-a)^2(s-a-1)
-- This leads to a Pell-like equation.

solve :: Integer
solve = sum [3*a + d | (a, d) <- solutions, 3*a + d <= 1000000000]
  where
    -- Generate solutions using the recurrence from Pell's equation
    -- For a,a,a+1: 16*area^2 = (a+1)(3a+1)(a-1) -- no wait
    -- Sides a,a,b where b = a+1 or b = a-1
    -- s = (2a+b)/2
    -- 16*A^2 = (2a+b)(2a-b)(b)(2a+b-2b) -- Heron's
    -- Actually 16A^2 = (a+a+b)(-a+a+b)(a-a+b)(a+a-b)
    --               = (2a+b)(b)(b)(2a-b)
    --               = b^2 * (2a+b)(2a-b)
    --               = b^2 * (4a^2 - b^2)
    -- For b=a+1: 16A^2 = (a+1)^2(4a^2-(a+1)^2) = (a+1)^2(3a^2-2a-1) = (a+1)^2(a-1)(3a+1)
    --           = (a+1)^2(a-1)(3a+1)
    -- For b=a-1: 16A^2 = (a-1)^2(4a^2-(a-1)^2) = (a-1)^2(3a^2+2a-1) = (a-1)^2(a+1)(3a-1)
    -- Need these to be perfect squares.
    solutions = gen1 ++ gen2
    -- Use Pell equation solutions
    gen1 = takeWhile (\(a,d) -> 3*a+d <= 1000000000) $ filter valid1
           [(a, 1) | a <- iterate (\a -> 4*a + round (sqrt (fromIntegral (12*a*a - 3) :: Double))) 1]
    gen2 = takeWhile (\(a,d) -> 3*a+d <= 1000000000) $ filter valid2
           [(a, -1) | a <- iterate (\a -> 4*a + round (sqrt (fromIntegral (12*a*a - 3) :: Double))) 1]
    valid1 (a, _) = let v = (a+1)*(a+1)*(a-1)*(3*a+1) in v > 0 && isSquare (v `div` 16) && v `mod` 16 == 0
    valid2 (a, _) = let v = (a-1)*(a-1)*(a+1)*(3*a-1) in v > 0 && isSquare (v `div` 16) && v `mod` 16 == 0
    isSquare n = let s = floor (sqrt (fromIntegral n :: Double)) in s*s == n

main :: IO ()
main = runBench 94 (return solve)
