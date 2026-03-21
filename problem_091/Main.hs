-- Problem 091: Right Triangles with Integer Coordinates
-- How many right triangles can be formed with vertices at (0,0), (x1,y1), (x2,y2)
-- where 0 <= x1,y1,x2,y2 <= 50?
-- Answer: 14234

module Main where

import Bench (runBench)

solve :: Int
solve = length
    [ ()
    | x1 <- [0..50], y1 <- [0..50]
    , x2 <- [0..50], y2 <- [0..50]
    , (x1, y1) /= (0, 0)
    , (x2, y2) /= (0, 0)
    , (x1, y1) < (x2, y2)
    , isRightAngle x1 y1 x2 y2
    ]
  where
    isRightAngle x1 y1 x2 y2 =
        let d1 = x1*x1 + y1*y1
            d2 = x2*x2 + y2*y2
            d3 = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)
            dots = [x1*x2 + y1*y2,
                    (x2-x1)*(-x1) + (y2-y1)*(-y1),
                    (x1-x2)*(-x2) + (y1-y2)*(-y2)]
        in any (== 0) dots

main :: IO ()
main = runBench 91 (return solve)
