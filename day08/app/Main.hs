module Main (main) where

import Lib
import Data.Char (ord)
import Data.List (intersect, transpose, zipWith4, zip4)

main :: IO ()
main = do
    text <- readFile "day8.in"
    let rows = lines text
    let trees = (map.map) (\x -> ord x - ord '0') rows
    let indexedTrees = map (zip [0..]) trees
    let hiddenLeft = [findHidden row [] True (-1) | row <- indexedTrees]
    let hiddenRight = [findHidden (reverse row) [] True (-1) | row <- indexedTrees]
    let hiddenHorizontally = [intersect l r | (l, r) <- zip hiddenLeft hiddenRight]
    let transposedTrees = map (zip [0..]) $ transpose trees
    let hiddenTop = [findHidden col [] True (-1) | col <- transposedTrees]
    let hiddenBottom = [findHidden (reverse col) [] True (-1) | col <- transposedTrees]
    let hiddenVertical = [intersect t b | (t, b) <- zip hiddenTop hiddenBottom]
    let positionHiddenHorizontally = concat [zip [p,p..] h | (p, h) <- zip [0..] hiddenHorizontally]
    let positionHiddenVertical = concat [zip h [p,p..] | (p, h) <- zip [0..] hiddenVertical]
    let totalHidden = intersect positionHiddenHorizontally positionHiddenVertical
    print $ (length rows)*(length rows) - (length totalHidden) -- First part
    let visibleLeft = [mapVisible x [] | x <- trees]
    let visibleRight = [reverse (mapVisible (reverse x) []) | x <- trees]
    let visibleTop = transpose [reverse (mapVisible (reverse x) []) | x <- transpose trees]
    let visibleBottom = transpose [ (mapVisible ( x) []) | x <- transpose trees]
    let visibleZip = zip4 visibleLeft visibleRight visibleTop visibleBottom
    let visibility = [zipWith4 (\w x y z -> w * x * y * z) l r t b | (l, r, t, b) <- visibleZip]
    let maxVisibility = foldr max 0 [foldr max 0 x | x <- visibility]
    print maxVisibility -- Second part