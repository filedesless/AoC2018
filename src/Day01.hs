module Day01 (day01a, day01b) where

import Data.Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

convert :: [String] -> [Int]
convert = map (read . filter (/= '+'))

day01a :: String -> Int
day01a = sum . convert . lines

takeUnseen :: IntSet -> [Int] -> Int
takeUnseen seen (x:xs) =
  if IntSet.member x seen
  then x
  else takeUnseen (IntSet.insert x seen) xs

day01b :: String -> Int
day01b = takeUnseen IntSet.empty . scanl (+) 0 . cycle . convert . lines
