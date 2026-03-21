-- Problem 087: Prime Power Triples
-- How many numbers below 50 million can be expressed as p^2 + q^3 + r^4?
-- Answer: 1097343

module Main where

import Bench (runBench)
import qualified Data.Set as Set

limit :: Int
limit = 50000000

primes :: [Int]
primes = sieve [2..7100]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > 7100 = p : xs
        | otherwise    = p : sieve [x | x <- xs, x `mod` p /= 0]

solve :: Int
solve = Set.size $ Set.fromList
    [ s
    | p4 <- takeWhile (< limit) [p^4 | p <- primes]
    , p3 <- takeWhile (\q -> p4 + q < limit) [q^3 | q <- primes]
    , p2 <- takeWhile (\r -> p4 + p3 + r < limit) [r^2 | r <- primes]
    , let s = p4 + p3 + p2
    ]

main :: IO ()
main = runBench 87 (return solve)
