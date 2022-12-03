module Main (main) where

import Lib (half, calculatePriorities)
import Data.List (intersect)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    text <- readFile "day3.in"
    let rucksacks = lines text
    let splitRucksacks = map half rucksacks
    let shared = [filter (\x-> x `elem` b) a !! 0 | (a, b) <- splitRucksacks]
    let priorities = calculatePriorities shared
    print (sum priorities) -- First part
    let groups = chunksOf 3 rucksacks
    let badges = [intersect (intersect a b) c !! 0 | [a, b, c] <- groups]
    let badgePriorities = calculatePriorities badges
    print (sum badgePriorities)