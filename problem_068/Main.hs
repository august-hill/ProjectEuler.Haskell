-- Problem 068: Magic 5-gon Ring
-- Find the maximum 16-digit string for a magic 5-gon ring.
-- Answer: 6531031914842725

module Main where

import Bench (runBench)
import Data.List (permutations, sort)

solve :: Integer
solve = maximum
    [ read (concatMap show flat) :: Integer
    | perm <- permutations [1..10]
    , let [a,b,c,d,e,f,g,h,i,j] = perm
    , let lines_ = [[a,f,g],[b,g,h],[c,h,i],[d,i,j],[e,j,f]]
    , let sums = map sum lines_
    , all (== head sums) sums
    , a == minimum (map head lines_)
    , let flat = concat lines_
    , length (show (read (concatMap show flat) :: Integer)) == 16
    ]

main :: IO ()
main = runBench 68 (return solve)
