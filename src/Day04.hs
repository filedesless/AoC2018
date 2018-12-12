module Day04 where

import Data.Time

data Shift = Shift { guardId :: Int
                   , shiftDay :: Day
                   , sleepIntervals :: [(Int, Int)] -- [min,max[
                   } deriving (Show, Eq)

getShifts :: String -> [Shift]
getShifts = error "Not implemented yet"

day04a :: String -> Int
day04a = error "Not implemented yet"

day04b :: String -> Int
day04b = error "Not implemented yet"
