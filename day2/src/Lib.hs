module Lib
    ( 
    findScore,
    changeSign,
    Sign
    ) where

data Sign = Rock | Paper | Scissor deriving (Enum, Eq, Show)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

compareSign :: Sign -> Sign -> Ordering
compareSign s1 s2 
    | s1 == s2 = EQ
    | s1 == Rock && s2 == Scissor = GT
    | s1 == Rock && s2 == Paper = LT
    | s1 == Scissor && s2 == Paper = GT
    | s1 == Scissor && s2 == Rock = LT
    | s1 == Paper && s2 == Rock = GT
    | s1 == Paper && s2 == Scissor = LT

changeSign :: Sign -> Sign -> Sign
changeSign s1 s2
    | s2 == Paper = s1
    | s1 == Rock = if s2 == Rock then Scissor else Paper
    | s1 == Paper = if s2 == Rock then Rock else Scissor 
    | s1 == Scissor = if s2 == Rock then Paper else Rock

findScore :: [Sign] -> Int
findScore s = do
    let (s1, s2) = tuplify2 s
    case compareSign s1 s2 of   
        GT -> 0 + fromEnum s2 + 1
        EQ -> 3 + fromEnum s2 + 1
        LT -> 6 + fromEnum s2 + 1
                                