module Lib where

import Data.List (break)

type Name = String
type Size = Int
data FSItem = File Name Size | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

parseInput :: FSZipper -> [String] ->  FSZipper
parseInput zipper ["$", "cd", "/"]  = fsGoToRoot zipper
parseInput zipper ["$", "cd", ".."]  = fsUp zipper
parseInput zipper ["$", "cd", x]  = (fsTo x zipper)
parseInput zipper ["$", "ls"]  = zipper
parseInput zipper ["dir", name]  = fsNewFile (Folder name []) zipper
parseInput zipper [size, name]  = fsNewFile (File name (read size)) zipper
parseInput zipper [_, _]  = zipper

-- cd ..
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- cd x
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

fsGoToRoot :: FSZipper -> FSZipper
fsGoToRoot (t, []) = (t, [])
fsGoToRoot z = fsGoToRoot (fsUp z)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- ls?
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = 
    (Folder folderName (item:items), bs)

itemSize :: FSItem -> Int-> Int
itemSize (Folder _ items) acc = foldr itemSize acc items
itemSize (File _ size) acc = acc + size

flatten :: [FSItem] -> [Int] -> [Int]
flatten fsItems acc = 
    case fsItems of
        [] -> acc
        (x:xs) -> case x of 
            (Folder _ items) -> [itemSize x 0] ++ acc ++ (flatten xs []) ++ (flatten items [])
            (File _ size) -> flatten xs acc