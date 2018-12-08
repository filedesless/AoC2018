module Day02 where

import Data.List (find)

hasN :: Int -> String -> Bool
hasN n [] = False
hasN n (x:xs) =
  length [ y | y <- xs, x == y ] == (pred n) ||
  hasN n [ y | y <- xs, x /= y ]

hasDuo :: String -> Bool
hasDuo = hasN 2

hasTrio :: String -> Bool
hasTrio = hasN 3

day02a :: String -> Int
day02a s =
  length (filter (hasDuo) (lines s)) *
  length (filter (hasTrio) (lines s))

hamming :: String -> String -> Int
hamming s1 s2 = length [ x | (x, y) <- zip s1 s2, x /= y ]

day02b :: String -> String
day02b s = case find ((==) 1 . uncurry hamming) [ (x, y) | x <- lines s, y <- lines s ] of
  Just (x, y) -> [ c1 | (c1, c2) <- zip x y, c1 == c2 ]
  Nothing -> error "Cannot find good box"
