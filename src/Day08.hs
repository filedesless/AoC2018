module Day08 where

import Data.Tree

readNumbers :: String -> [Int]
readNumbers s = [ read c :: Int | c <- words s ]

buildTree :: [Int] -> (Int, Tree [Int])
buildTree (n:m:rest) = (total + m + 2, Node (take m $ drop total rest) forest)
  where
    (total, forest) =
      foldl (\(total, forest) _ ->
               let todo = drop total $ take (length rest - m) rest in
                 let (i, Node label sub) = buildTree todo in
                   (total + i, forest ++ [Node label sub])) (0, [])  [1..n]

day08a :: String -> Int
day08a s = undefined

day08b :: String -> Int
day08b s = undefined
