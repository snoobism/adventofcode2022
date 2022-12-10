module Main (main) where

import Lib
import qualified Data.Set as Set 

main :: IO ()
main = do
    text <- readFile "day9.in"
    let directions = [(x, (read::String->Int) y) | [x, y] <- map words (lines text)]
    let headTrail = reverse $ headDirections directions (0, 0) []
    let tailTrail = reverse $ tailDirections headTrail (0, 0) []
    print $ length $ Set.fromList tailTrail -- First part
    let nTrail = applyN headTrail 9 
    print $ length $ Set.fromList nTrail -- Second part
