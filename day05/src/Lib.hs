module Lib
    ( moveCrates,
    doNothing
    ) where

enumerate x = zip [1..] x

doNothing s = s

moveCrates :: [String] -> [[Int]] -> (String -> String) -> [String]
moveCrates crates moves mover =
    case moves of
        [] -> crates
        [move, from, to]:xs -> moveCrates [
            if index == from then 
                drop move stack else 
            if index == to then
                (mover (take move (crates !! (from-1)))) ++ stack
            else stack | (index, stack) <- enumerate crates] xs mover
