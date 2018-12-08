module Main where

import Text.Printf (printf)
import Data.Text (justifyRight, unpack, pack)
import Day01 (day01a, day01b)
import Day02 (day02a)

run :: (Show a) => Integer  -> String -> (String -> a) -> IO ()
run day part function =
  readFile ("input/" ++ unpack (justifyRight 2 '0' (pack (show day))) ++ ".txt")
  >>= printf "Day %02d %s: %s\n" day part . (show . function)

main :: IO ()
main =
  run 1 "a)" day01a >>
  run 1 "b)" day01b >>
  run 2 "a)" day02a

