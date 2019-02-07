module Day09 where

import Data.Maybe
import Deque
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap

data Game = Game { getScores :: IntMap Int
                 , getMarble :: Deque Int
                 } deriving (Show, Eq)

game = Game IntMap.empty (Deque [] [0])

rot :: Int -> Deque a -> Deque a
rot n d = iterate (if n < 0 then shiftRight else shiftLeft) d !! (abs n)

turn' :: Game -> Int -> Int -> Int -> Game
turn' game@(Game scores marble) n m i
  | i == m + 1    = game
  | mod i 23 == 0 = turn' (Game score' $ Deque.tail lefty) n m $ succ i
  | otherwise     = turn' (Game scores $ cons i $ rot 2 marble) n m $ succ i
  where
    score' = IntMap.insertWith (+) (mod i n) newval scores
    newval = sum $ catMaybes [Just i, Deque.head lefty]
    lefty = rot (-7) marble

turn :: Game -> Int -> Int -> Game
turn game n m = turn' game n m 1

highest :: Int -> Int -> Int
highest n m = IntMap.foldr' max 0 . getScores $ turn game n m

getInput :: String -> (Int, Int)
getInput s = (read (Prelude.head w) :: Int, read (w !! 6) :: Int)
  where w = words s

day09a :: String -> Int
day09a = uncurry highest . getInput

day09b :: String -> Int
day09b = uncurry highest . fmap (* 100) . getInput
