module Main (main) where
import Data.List.Split

main :: IO ()
main = do
    text <- readFile "day1.in"
    let textLines = lines text
    let groupedLines = splitOn [""] textLines
    let intLines = [map read x::[Int] | x <- groupedLines]
    let sums = [sum x | x <- intLines]
    let m = foldr max 0 sums
    print(m) -- First Part
    print(sum $ take 3 $ sortBy (flip compare) sums) -- Second Part