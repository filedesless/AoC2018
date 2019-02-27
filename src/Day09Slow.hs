module Day09Slow where

import Data.Vector

-- doubly linked list ish
data Node = Node
  { prev :: Int
  , next :: Int } deriving (Show, Eq)
type Marbles = Vector Node
type Players = Vector Int

d marbles = aux marbles first (prev first)
  where
    first = marbles ! 0
    aux marbles (Node p n) u
      | n == u    = [n]
      | otherwise = n : aux marbles (marbles ! n) u

insertAfter :: Marbles -> Int -> Int -> Marbles
insertAfter marbles pos val = marbles // [current, newnode, nxtnode]
  where
    current = (pos, Node (prev marble) val)
    newnode = (val, Node pos (next marble))
    nxtnode = (nxt, Node val (next oldnxt))
    oldnxt = marbles ! next marble
    marble = marbles ! pos
    nxt = next marble

remove :: Marbles -> Int -> Marbles
remove marbles pos = marbles // [oldnode, nxtnode]
  where
    oldnode = (old, Node (prev $ marbles ! old) nxt)
    nxtnode = (nxt, Node old (next $ marbles ! nxt))
    old = prev $ marbles ! pos
    nxt = next $ marbles ! pos

turn :: Int -> Int -> Int -> Int -> Marbles -> Players -> (Marbles, Players)
turn n m i j marbles players
  | i == n + 1    = (marbles, players)
  | mod i 23 == 0 = turn n m (i + 1) (next $ removed ! removed_ix) removed newscore
  | otherwise     = turn n m (i + 1) (next $ inserted ! inserted_ix) inserted players
  where
    inserted = insertAfter marbles inserted_ix i
    inserted_ix = next $ marbles ! j
    removed = remove marbles removed_ix
    removed_ix = iterate (prev . (marbles !)) j !! 7
    newscore = players // [(player, (players ! player) + removed_ix + i)]
    player = mod i m

play :: Int -> Int -> (Marbles, Players)
play n m = turn n m 1 1 marbles players
  where
    marbles = fromList $ Node 1 1 : Node 0 0 : [ Node 0 0 | _ <- [2..n] ]
    players = Data.Vector.replicate m 0

getInput :: String -> (Int, Int)
getInput s = (read (Prelude.head w) :: Int, read (w !! 6) :: Int)
  where w = words s

highest :: Int -> Int -> Int
highest m n = Data.Vector.maximum . snd $ play n m

day09a :: String -> Int
day09a = uncurry highest . getInput

day09b :: String -> Int
day09b = uncurry highest . fmap (* 100) . getInput
