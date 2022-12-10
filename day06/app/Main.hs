module Main (main) where

import Lib

main :: IO ()
main = do
    text <- readFile "day6.in"
    let signals = lines text
    let markers1 = map (\x -> findMarker x 4 4) signals 
    print markers1 -- First part
    let markers2 = map (\x -> findMarker x 14 14) signals 
    print markers2 -- Second part