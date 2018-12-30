module Day05 where

import Data.List
import Data.Function( on )

react :: Char -> Char -> Bool
react x y = 32 == abs (fromEnum x - fromEnum y)

removePairs :: String -> String
removePairs [ ] = [ ]
removePairs [x] = [x]
removePairs (x:y:rest)
  | react x y = removePairs rest
  | otherwise = x : removePairs (y : rest)

reactAll :: String -> String
reactAll s
  | all ((== 1) . length) $ groupBy react s = delete '\n' s
  | otherwise = reactAll $ removePairs s

day05a :: String -> Int
day05a = length . reactAll

day05b :: String -> Int
day05b s = minimum $ map (length . reactAll . possibility) ['a'..'z']
  where
    possibility l = [ c | c <- (delete '\n' s), not $ react l c, c /= l ]
