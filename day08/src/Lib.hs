module Lib
    ( countVisible,
    mapVisible,
    findHidden
    ) where

countVisible :: [Int] -> Int -> Int -> Int
countVisible item current acc = 
    case item of
        [] -> acc
        (x:xs) ->
            if (x >= current) then 
                acc+1
            else  
                countVisible xs current (acc+1)

mapVisible :: [Int] -> [Int] -> [Int]
mapVisible [] acc = acc
mapVisible (x:xs) acc = (countVisible xs x 0):acc ++ mapVisible xs []

findHidden :: [(Int, Int)] -> [Int] -> Bool -> Int -> [Int]
findHidden [] acc isIncreasing valleyStart = acc
findHidden ((i, x):xs) acc isIncreasing valleyStart = 
    case isIncreasing of
        True -> if (x > valleyStart) then 
                -- if it's still increasing
                    findHidden xs acc True x
                else
                -- if it starts decreasing
                    findHidden xs (i:acc) False valleyStart
        False -> if (x > valleyStart) then 
                    -- if it starts increasing
                    findHidden xs acc True x
                else if (x == valleyStart) then 
                    findHidden xs (i:acc) True x
                else 
                    -- still decreasing
                    findHidden xs (i:acc) False valleyStart
