-- Problem 017: Number Letter Counts
-- Count letters used writing 1-1000 in British English (no spaces or hyphens).
-- Answer: 21124

module Main where

import Bench (runBench)

letterCount :: Int -> Int
letterCount n
  | n == 1000 = 3 + 8                    -- "one" + "thousand"
  | n >= 100  = ones (n `div` 100) + 7   -- "hundred"
              + if n `mod` 100 > 0 then 3 + below100 (n `mod` 100) else 0  -- "and"
  | otherwise = below100 n
  where
    below100 x
      | x >= 20   = tens (x `div` 10) + ones (x `mod` 10)
      | x >= 10   = teens (x - 10)
      | otherwise  = ones x
    ones  x = [0, 3, 3, 5, 4, 4, 3, 5, 5, 4] !! x
    teens x = [3, 6, 6, 8, 8, 7, 7, 9, 8, 8] !! x  -- ten..nineteen
    tens  x = [0, 0, 6, 6, 5, 5, 5, 7, 6, 6] !! x  -- twenty..ninety

solve :: Int
solve = sum $ map letterCount [1..1000]

main :: IO ()
main = runBench 17 (return solve)
