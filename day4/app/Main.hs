module Main (main) where

import Lib
import Data.List (intersect)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    text <- readFile "day4.in"
    let rows = lines text
    let pairs = [splitOn "," row | row <- rows]
    let sectionsStr = [map (splitOn "-") pair | pair <- pairs]
    let sectionsInt = [[map (read::String->Int) x | x <- section] | section <- sectionsStr] -- [[]]
    let contained = foldr (\[[a1, b1], [a2, b2]] -> if (a1 <= a2 && b2 <= b1) || (a2 <= a1 && b1 <= b2) then (+1) else (+0)) 0 sectionsInt
    print contained -- First Part
    let overlap = foldr (\[[a1, b1], [a2, b2]] -> if (intersect [a1..b1] [a2..b2] /= []) then (+1) else (+0)) 0 sectionsInt
    print overlap -- Second Part


