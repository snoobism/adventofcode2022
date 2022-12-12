module Main (main) where

import Lib
import qualified Data.Map as Map
import Algorithm.Search (bfs)

main :: IO ()
main = do
    text <- readFile "day12.in"
    let hmap = lines text
    let coords = [(x, y, d)| (y, row) <- zip [0..] hmap, (x, d) <- zip [0..] row]
    let coordsInt = map (\(x, y, d)->(x,y,convert d)) coords
    let start = head $ map (\(x,y,d) -> ((x,y), convert 'a')) $ filter (\(x, y, d) -> d == 'S') coords
    let end = head $ map (\(x,y,d) -> ((x,y), convert 'z')) $ filter (\(x, y, d) -> d == 'E') coords
    let directions = map (getDirections coords (length (head hmap), length hmap)) coords 
    let graph = Map.fromList directions
    let path = extractPath $ bfs (graph Map.!) (== end) start
    print $ length path -- First part
    let lowestTiles = map fst $ filter (\(((x,y), d), l) -> d == ord 'a') directions
    let possiblePathLengths = filter (/=0) [length $ extractPath $ bfs (graph Map.!) (== end) t | t <- lowestTiles]
    let minimalPathLength = foldr1 min possiblePathLengths 
    print minimalPathLength -- Second part