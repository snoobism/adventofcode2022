module Main (main) where

import Lib
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    text <- readFile "day10.in"
    let cmds = map words (lines text)
    let cmdsWithDelay = addDelay cmds []
    let registerValues = scanl parseCmds 1 cmdsWithDelay
    let signalStrength = [x * (registerValues !! (x-1)) | x <- [20, 60..220]]
    print $ sum signalStrength -- First part
    let timestampRegister = zip [0..] registerValues
    let crt = map (\(cycle, reg) -> if abs(reg - (cycle`mod`40)) <= 1 then '#' else '.') timestampRegister 
    let crtLines = chunksOf 40 crt
    mapM_ putStrLn crtLines -- Second part