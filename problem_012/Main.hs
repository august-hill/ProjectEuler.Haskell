-- Problem 012: Highly Divisible Triangular Number
-- Find the first triangle number with over 500 divisors.
-- Answer: 76576500

module Main where

import Bench (runBench)

triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [1..]

countDivisors :: Integer -> Int
countDivisors n = length [d | d <- [1..isqrt n], n `mod` d == 0] * 2
                  - (if isqrt n * isqrt n == n then 1 else 0)
  where isqrt = floor . sqrt . fromIntegral

solve :: Integer
solve = head $ filter (\t -> countDivisors t > 500) triangleNumbers

main :: IO ()
main = runBench 12 (return solve)
