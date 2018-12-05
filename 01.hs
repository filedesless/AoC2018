import Text.Printf
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

getNum :: String -> String
getNum (h:t)
  | h == '+' = t
  | otherwise = h : t

convert :: [String] -> [Int]
convert = map (read . getNum)

q1 :: IO ()
q1 =
  readFile "01-1.txt" >>= printf "Sol 1: %d\n" .
  sum . convert . lines

takeUnseen :: IntSet -> [Int] -> Int
takeUnseen seen (x:xs) =
  if IntSet.member x seen
  then x
  else takeUnseen (IntSet.insert x seen) xs

q2 :: IO ()
q2 =
  readFile "01-1.txt" >>= printf "Sol 2: %d\n" .
  takeUnseen IntSet.empty . scanl1 (+) . cycle . convert . lines

main :: IO ()
main = q1 >> q2
