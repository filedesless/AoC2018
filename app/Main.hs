module Main where

import Text.Printf (printf)
import Data.Text (justifyRight, unpack, pack)
import Day01 (day01a, day01b)
import Day02 (day02a, day02b)
import Day03 (day03a, day03b)
import Day04 (day04a, day04b)

run :: (Show a) => Integer  -> String -> (String -> a) -> IO ()
run day part function =
  readFile ("input/" ++ unpack (justifyRight 2 '0' (pack (show day))) ++ ".txt")
  >>= printf "Day %02d %s: %s\n" day part . (show . function)

main :: IO ()
main =
  run 1 "a)" day01a >>
  run 1 "b)" day01b >>
  run 2 "a)" day02a >>
  run 2 "b)" day02b >>
  run 3 "a)" day03a >>
  run 3 "b)" day03b >>
  run 4 "a)" day04a
