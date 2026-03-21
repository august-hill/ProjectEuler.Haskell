-- Problem 079: Passcode Derivation
-- Determine the shortest passcode from the login attempts.
-- Answer: 73162890

module Main where

import Bench (runBench)
import Data.List (nub, (\\))
import qualified Data.Set as Set

attempts :: [[Int]]
attempts = map (map (read . (:[]))) $ nub
    ["319","680","180","690","129","620","762","689","762","318"
    ,"368","710","720","710","629","168","160","689","716","731"
    ,"736","729","316","729","729","710","769","290","719","680"
    ,"318","389","162","289","162","718","729","319","790","680"
    ,"890","362","319","760","316","729","380","319","728","716"]

-- Topological sort based on ordering constraints
solve :: Integer
solve = read $ concatMap show $ topoSort allDigits edges
  where
    allDigits = nub $ concat attempts
    edges = nub [(a, b) | [a, b, _] <- attempts] ++
            nub [(b, c) | [_, b, c] <- attempts] ++
            nub [(a, c) | [a, _, c] <- attempts]
    topoSort [] _ = []
    topoSort nodes es =
        let noIncoming = [n | n <- nodes, n `notElem` map snd es]
            next = head noIncoming
            remaining = nodes \\ [next]
            newEdges = filter (\(a, _) -> a /= next) es
        in next : topoSort remaining newEdges

main :: IO ()
main = runBench 79 (return solve)
