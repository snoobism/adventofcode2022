module Lib
    ( move, 
    headDirections,
    tailDirections, 
    applyN
    ) where

move :: (String, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move (_, 0) _ trail = trail
move ("R", num) (x,y) trail = (move ("R", num - 1) (x+1, y) ((x+1,y):trail)) 
move ("L", num) (x,y) trail = (move ("L", num - 1) (x-1, y) ((x-1,y):trail))
move ("U", num) (x,y) trail = (move ("U", num - 1) (x, y+1) ((x,y+1):trail)) 
move ("D", num) (x,y) trail = (move ("D", num - 1) (x, y-1) ((x,y-1):trail)) 

headDirections :: [(String, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
headDirections [] _ trail = trail
headDirections (m:ms) pos trail = do
    let moveTrail@(x:xs) = move m pos trail
    headDirections ms x (move m pos trail)

tailDirections :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
tailDirections [] pos trail = (pos:trail) 
tailDirections ((hx, hy):ms)  pos@(tx, ty) trail = do
    let (nextX, nextY) = case ((abs(hx-tx) <= 1), (abs(hy-ty) <= 1)) of
            (True, True) -> (tx, ty)
            (False, True) -> ((hx+tx) `div` 2, hy)
            (True, False) -> (hx, (hy+ty) `div` 2)
            (False, False) -> ((hx+tx) `div` 2, (hy+ty) `div` 2)
    tailDirections ms (nextX, nextY) (pos:trail)

applyN :: [(Int, Int)] -> Int -> [(Int, Int)]
applyN headTrail 0 = headTrail
applyN headTrail n = applyN (reverse $ tailDirections headTrail (0, 0) []) (n-1)
    