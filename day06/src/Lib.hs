module Lib
    ( findMarker
    ) where

import Data.List.Unique (allUnique)

findMarker :: String -> Int -> Int -> Int
findMarker signal size position =
    case allUnique $ take size signal of
        True -> position
        False -> findMarker (tail signal) size (position + 1)
    
