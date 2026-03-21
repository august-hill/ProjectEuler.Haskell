-- Problem 095: Amicable Chains
-- Find the smallest member of the longest amicable chain with no element exceeding 10^6.
-- Answer: 14316

module Main where

import Bench (runBench)
import Data.IORef
import qualified Data.Array.IO as A
import qualified Data.Array.Unboxed as U

limit :: Int
limit = 1000000

-- Build sum-of-proper-divisors sieve (same O(N log N) algorithm as all other languages)
buildSieve :: IO (U.UArray Int Int)
buildSieve = do
    arr <- A.newArray (0, limit) 0 :: IO (A.IOUArray Int Int)
    -- Initialize all entries >= 2 to 1 (every n>1 has 1 as a divisor)
    mapM_ (\i -> A.writeArray arr i 1) [2..limit]
    -- Sieve: for each i >= 2, add i to all multiples of i
    mapM_ (\i -> mapM_ (\j -> do
        v <- A.readArray arr j
        A.writeArray arr j (v + i)
        ) [2*i, 3*i .. limit]
        ) [2..limit]
    A.freeze arr

solve :: U.UArray Int Int -> Int
solve sumDiv = go 2 emptyVisited 0 0
  where
    emptyVisited = U.listArray (0, limit) (repeat False) :: U.UArray Int Bool

    go start visited bestLen bestMin
        | start > limit = bestMin
        | visited U.! start = go (start + 1) visited bestLen bestMin
        | otherwise =
            let (chain, _) = followChain start [] emptySet
                (newBestLen, newBestMin, newVisited) = processChain chain start visited bestLen bestMin
            in go (start + 1) newVisited newBestLen newBestMin

    emptySet = U.listArray (0, limit) (repeat False) :: U.UArray Int Bool

    followChain n acc inChain
        | n <= 0 || n > limit = (acc, inChain)
        | inChain U.! n = (acc, inChain)
        | otherwise =
            let inChain' = inChain U.// [(n, True)]
            in followChain (sumDiv U.! n) (acc ++ [n]) inChain'

    processChain chain start visited bestLen bestMin =
        let visited' = foldl (\v x -> if x >= 0 && x <= limit then v U.// [(x, True)] else v) visited chain
            next = if null chain then 0 else sumDiv U.! last chain
        in if next > 0 && next <= limit && any (== next) chain
           then let cycleStart = length (takeWhile (/= next) chain)
                    cycle = drop cycleStart chain
                    cycleLen = length cycle
                    cycleMin = minimum cycle
                in if cycleLen > bestLen
                   then (cycleLen, cycleMin, visited')
                   else (bestLen, bestMin, visited')
           else (bestLen, bestMin, visited')

main :: IO ()
main = do
    sieve <- buildSieve
    runBench 95 (return (solve sieve))
