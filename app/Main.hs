module Main where

import Text.Printf
import Day01
import Day02

main :: IO ()
main =
  readFile "input/01-1.txt" >>= printf "Day 01 a): %d\n" . day01a >>
  readFile "input/01-1.txt" >>= printf "Day 01 b): %d\n" . day01b >>
  readFile "input/02-1.txt" >>= printf "Day 02 a): %d\n" . day02a
