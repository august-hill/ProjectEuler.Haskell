-- Problem 083: Path Sum: Four Ways
-- Find minimum path sum in an 80x80 matrix moving in all four directions.
-- Answer: 425185

module Main where

import Bench (runBench)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

parseMatrix :: String -> [[Int]]
parseMatrix = map (map read . words . map (\c -> if c == ',' then ' ' else c)) . lines

type Pos = (Int, Int)

dijkstra :: [[Int]] -> Int
dijkstra matrix = go (Set.singleton (matrix !! 0 !! 0, (0, 0))) Set.empty
  where
    rows = length matrix
    cols = length (head matrix)
    go queue visited
        | pos == (rows - 1, cols - 1) = dist
        | Set.member pos visited = go rest visited
        | otherwise = go (foldl (\q (d, p) -> Set.insert (d, p) q) rest neighbors) (Set.insert pos visited)
      where
        ((dist, pos), rest) = Set.deleteFindMin queue
        (r, c) = pos
        neighbors = [(dist + matrix !! r' !! c', (r', c'))
                    | (dr, dc) <- [(-1,0),(1,0),(0,-1),(0,1)]
                    , let r' = r + dr
                    , let c' = c + dc
                    , r' >= 0, r' < rows, c' >= 0, c' < cols
                    , not (Set.member (r', c') visited)
                    ]

main :: IO ()
main = runBench 83 $ do
    contents <- readFile "matrix.txt"
    return (dijkstra (parseMatrix contents))
