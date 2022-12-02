module Main (main) where

import Lib
import Text.Regex.TDFA
import Data.Char (ord)

main :: IO ()
main = do
    text <- readFile "day2.in"
    let input = map tail (text =~ "([ABC]) ([XYZ])+")::[[String]]
    let ints = [map ord (concat x) | x <- input]
    let norm = [[a-65, b-88] | [a,b] <- ints]
    let enums = [[toEnum y::Sign | y <- x] | x <- norm]
    let scores = map findScore enums
    print(sum scores) -- First Part
    let rigged = [[a, changeSign a b] | [a, b] <- enums]
    print(rigged)
    print(sum $ map findScore rigged) -- Second part