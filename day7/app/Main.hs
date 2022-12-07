module Main (main) where

import Lib

main :: IO ()
main = do
    text <- readFile "day7.in"
    let rows = lines text
    let input = map words rows
    let root = (Folder "/" [])
    let fs = foldl parseInput (root, []) input
    let tree = fst $ fsGoToRoot fs
    let rootSize = itemSize tree 0
    let folderSizes = flatten [tree] []
    print $ sum $ filter (<=100000) folderSizes -- First part
    let deletionCandidates = filter (\x -> (rootSize - 40000000 <= x)) folderSizes
    print $ foldr1 min deletionCandidates -- Second part
