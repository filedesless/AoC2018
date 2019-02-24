module Main (main) where

import Criterion.Main
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Day01( day01b_fast, day01b_slow )

main :: IO ()
main = do
  day01Input <- readFile "input/01.txt"
  defaultMain [
    bgroup "Day01" [ bench "day01b_fast" $ whnf day01b_fast day01Input
                   , bench "day01b_slow" $ whnf day01b_slow day01Input
                   ]
    ]
