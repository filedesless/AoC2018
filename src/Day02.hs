module Day02 where

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

day02b :: String -> Int
day02b = error "Not implemented yet"
