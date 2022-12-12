module Main (main) where

import Lib

main :: IO ()
main = do
    text <- readFile "day11.in"
    let rows = lines text
    let actions = (map.map) ((drop 1).(splitOn ":")) (map (drop 1) $ splitOn [""] rows) 
    let monkeys = map parseActions actions
    let monkeysPart1 = rounds monkeys 0 20 (`div`3)
    print $ monkeyBusiness monkeysPart1 -- First part
    let reduction = product (map (\(_,_,m,_,_,_) -> m) monkeys)
    let monkeysPart2 = rounds monkeys 0 10000 (`mod` reduction)
    print $ monkeyBusiness monkeysPart2 -- Second part