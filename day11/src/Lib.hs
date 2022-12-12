module Lib
    ( parseActions, 
    splitOn,
    rounds,
    monkeyBusiness
    ) where
import Data.List.Split (splitOn)
import Data.List (sortBy)

parseFunction :: [String] -> (Integer -> Integer)
parseFunction ["*", "old"] = (^2)
parseFunction ["+", "old"] = (*2)
parseFunction ["+", old] = (+ (read old))
parseFunction ["*", old] = (* (read old))

parseActions :: [[String]] -> ([Integer], (Integer -> Integer), Integer, Integer, Integer, Integer)
parseActions [[worries], [action], [condition], [true], [false]] = do
    let parsedWorries = map read $ splitOn "," worries
    let parsedAction = parseFunction $ drop 4 $ splitOn " " action
    let parsedCondition = read $ last $ splitOn " " condition
    let parsedTrue = read $ last $ splitOn " " true
    let parsedFalse = read $ last $ splitOn " " false
    (parsedWorries, parsedAction, parsedCondition, parsedTrue, parsedFalse, 0)

throwFromTo :: [([Integer], (Integer -> Integer), Integer, Integer, Integer, Integer)] -> Integer -> Integer -> Integer -> [([Integer], (Integer -> Integer), Integer, Integer, Integer, Integer)] 
throwFromTo monkeys item from to = [if i == from then ((tail a),b,c,d,e,f+1) else if i == to then (a ++ [item],b,c,d,e,f) else m | (i,m@(a,b,c,d,e,f)) <- zip [0..] monkeys] 

rounds :: [([Integer], (Integer -> Integer), Integer, Integer, Integer, Integer)] -> Int -> Int -> (Integer->Integer) -> [([Integer], (Integer -> Integer), Integer, Integer, Integer, Integer)]
rounds monkeys current 0 factor = monkeys
rounds monkeys current n factor = do
    let monkey@(worries, action, condition, true, false, inspected) = monkeys !! current
    if worries == [] then
        rounds monkeys ((current + 1)`mod`(length monkeys)) (if (current + 1)`mod`(length monkeys) == 0 then (n-1) else n) factor
    else do
        let item = head worries
        let result = factor (action item)
        let check = result `rem` condition
        let monkeysAfterThrow = if check == 0 then
                                    throwFromTo monkeys result (toInteger current) true
                                else
                                    throwFromTo monkeys result (toInteger current) false
        rounds monkeysAfterThrow current n factor
        
monkeyBusiness :: [([Integer], (Integer -> Integer), Integer, Integer, Integer, Integer)] -> Integer
monkeyBusiness monkeys = product $ take 2 (sortBy (flip compare) (map (\(_,_,_,_,_,v) -> v) monkeys))