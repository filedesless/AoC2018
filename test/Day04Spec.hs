module Day04Spec (spec) where

import Day04
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Time

aDay :: Int -> Int -> Day
aDay = fromGregorian 1518

expectedShifts :: [Shift]
expectedShifts = [
  Shift 10 (aDay 11 01) [(05, 25), (30, 55)],
  Shift 99 (aDay 11 02) [(40, 50)],
  Shift 10 (aDay 11 03) [(24, 29)],
  Shift 99 (aDay 11 04) [(36, 46)],
  Shift 99 (aDay 11 05) [(45, 55)]
  ]

spec :: Spec
spec = do
    describe "getShifts" $
      it "should work with basic examples" $ do
        input <- readFile "input/04.test.txt"
        getShifts input `shouldBe` expectedShifts
