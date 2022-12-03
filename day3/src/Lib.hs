module Lib
    ( half,
    calculatePriorities
    ) where

import Data.Char (isUpper, ord)

half :: [a] -> ([a], [a])
half a = splitAt (length a `div` 2) a

calculatePriorities :: [Char] -> [Int]
calculatePriorities c = [if isUpper x then ord x - 38 else ord x - 96| x <- c]