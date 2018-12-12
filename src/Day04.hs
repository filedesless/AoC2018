module Day04 where

import Data.Time
import Data.List
import Data.List.Split

data Shift = Shift { guardId :: Int
                   , shiftDay :: Day
                   , sleepIntervals :: [(Int, Int)] -- [min,max[
                   } deriving (Show, Eq)


linesOfDay :: [String] -> Day -> [String]
linesOfDay sorted day = filter (showGregorian day `isInfixOf`) sorted

getGid :: [String] -> Day -> Int
getGid sorted day = case find (isInfixOf "Guard") $ linesOfDay sorted day of
  Just line -> read $ splitOneOf "# " line !! 4
  Nothing -> getGid (reverse sorted) $ pred day

getSleepIntervals :: [String] -> [(Int, Int)]
getSleepIntervals sorted = [ (read fall, read wake) | (fall, wake) <- group2 xs ]
  where
    group2 [] = []
    group2 (fall:wake:res) = (fall, wake) : group2 res
    xs = [ drop 3 $ take 5 $ words line !! 1 |
           line <- sorted, " 00:" `isInfixOf` line,
           "falls" `isInfixOf` line || "wakes" `isInfixOf` line ]

getShifts :: String -> [Shift]
getShifts input = [ Shift (getGid sorted day) day (sleeps day) | day <- daysOf sorted ]
  where
    sleeps = getSleepIntervals . linesOfDay sorted
    daysOf = nub . map (extractDay . getDay)
    getDay = splitOn "-" . head . words
    sorted = sort $ lines input
    extractDay [x, y, z] = fromGregorian (read $ drop 1 x) (read y) (read z)

day04a :: String -> Int
day04a = error "Not implemented yet"

day04b :: String -> Int
day04b = error "Not implemented yet"
