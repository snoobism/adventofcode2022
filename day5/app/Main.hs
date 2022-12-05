module Main (main) where

import Lib
import Data.List.Split (splitOn)
import Text.Regex.TDFA

main :: IO ()
main = do
    text <- readFile "day5.in"
    let rows = lines text
    let [stacks, moves] = splitOn [""] rows
    let stackClean =  map (filter (/=' ')) [map (!! x) (init stacks) | x <- [1,5..35]]
    let movesClean = (map.map) (read::String->Int) [getAllTextMatches (move =~ "[0-9]+")::[String] | move <- moves]
    let mover9000 = moveCrates stackClean movesClean reverse
    print $ foldr (++) (map head mover9000) [] -- First part
    let mover9001 = moveCrates stackClean movesClean doNothing
    print $ foldr (++) (map head mover9001) [] -- Second part