module Lib
    ( getXY,
    getDirections,
    ord,
    convert, 
    extractPath
    ) where
        
import Data.Char (ord)
import Algorithm.Search (dijkstra, pruning)
import Data.Maybe (fromJust)

getXY :: [(Int, Int, Char)] -> (Int, Int) -> Int -> Int -> (Int, Int, Char)
getXY c (sx, sy) x y = if (x >= 0 && y >= 0 && y < sy && x < sx) then (c !! (x + y * sx)) else (-1, -1, '0')

-- LRUP
getDirections :: [(Int, Int, Char)] -> (Int, Int) -> (Int, Int, Char) -> (((Int, Int), Int), [((Int, Int), Int)])
getDirections coords (sx, sy) (x, y, d) = do
    let neighbours = [getXY coords (sx, sy) (x+dx) y | dx <- [-1, 1]] ++ [getXY coords (sx, sy) x (y+dy) | dy <- [-1,1]]  
    let values = filter (/=((-1, -1), '0')) $ map (\(x, y, c) -> ((x, y), c)) neighbours
    let filtered = (((x, y), convert d), filter (\((fx,fy), fd)-> (abs((convert d) - fd) <= 1 || (convert d > fd))) $ map (\(c, v) -> (c, convert v)) values)
    filtered

convert :: Char -> Int
convert c = if c == 'S' then ord 'a' else if c == 'E' then ord 'z' else ord c

extractPath :: Maybe [a] -> [a]
extractPath p =
    case p of 
        (Just p) -> fromJust (Just p)
        Nothing -> []