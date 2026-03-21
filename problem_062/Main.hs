-- Problem 062: Cubic Permutations
-- Find smallest cube for which exactly 5 permutations of its digits are also cubes.
-- Answer: 127035954683

module Main where

import Bench (runBench)
import Data.List (sort, groupBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

digitSig :: Integer -> String
digitSig = sort . show

solve :: Integer
solve = minimum group5
  where
    cubes = [n^3 | n <- [1..10000] :: [Integer]]
    grouped = Map.fromListWith (++) [(digitSig c, [c]) | c <- cubes]
    group5 = head [cs | cs <- Map.elems grouped, length cs == 5]

main :: IO ()
main = runBench 62 (return solve)
