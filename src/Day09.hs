module Day09 where

import Data.Maybe
import Data.CircularList

data Game = Game { getScores :: CList Int
                 , getMarble :: CList Int
                 } deriving (Show, Eq)

turn :: Game -> Int -> Game
turn (Game scores marble) i
  | mod i 23 == 0  = Game (rotR score) $ removeR lefty
  | otherwise      = Game (rotR scores) $ insertL i $ rotR marble
  where
    score = update (sum $ catMaybes [Just i, focus scores, focus lefty]) scores
    lefty = rotNL 7 marble

highest :: Int -> Int -> Int
highest n m = foldlR max 0 . getScores $ foldl turn game [1..m]
  where game = Game (fromList $ replicate n 0) (singleton 0)

getInput :: String -> (Int, Int)
getInput s = (read (head w) :: Int, read (w !! 6) :: Int)
  where w = words s

day09a :: String -> Int
day09a = uncurry highest . getInput

day09b :: String -> Int
day09b = uncurry highest . fmap (* 100) . getInput
