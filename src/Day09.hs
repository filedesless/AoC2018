module Day09 where

import Data.Maybe
import Data.CircularList
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap

data Game = Game { getScores :: IntMap Int
                 , getMarble :: CList Int
                 } deriving (Show, Eq)

game = Game IntMap.empty (singleton 0)

turn :: Game -> Int -> Int -> Int -> Game
turn game@(Game scores marble) n m i
  | i == m + 1    = game
  | mod i 23 == 0 = turn (Game score' $ removeR lefty)  n m $ succ i
  | otherwise     = turn (Game scores $ insertL i $ rotR marble) n m $ succ i
  where
    score' = IntMap.insertWith (+) (mod i n) newval scores
    newval = sum $ catMaybes [Just i, focus lefty]
    lefty = rotNL 7 marble

highest :: Int -> Int -> Int
highest n m = IntMap.foldr' max 0 . getScores $ turn game n m 1

getInput :: String -> (Int, Int)
getInput s = (read (head w) :: Int, read (w !! 6) :: Int)
  where w = words s

day09a :: String -> Int
day09a = uncurry highest . getInput

day09b :: String -> Int
day09b = uncurry highest . fmap (* 100) . getInput
