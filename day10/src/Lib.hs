module Lib
    ( 
        parseCmds, 
        addDelay
    ) where

parseCmds :: Int -> [String] -> Int
parseCmds register ["noop"] = register
parseCmds register ["addx", x]  = register + (read x::Int)

addDelay :: [[String]] -> [[String]] -> [[String]]
addDelay [] acc = acc
addDelay (x:xs) acc = 
    case x of
        ["noop"] -> (x:(addDelay xs acc))
        ["addx", _] -> (["noop"]:x:(addDelay xs acc))