module Day04Spec (spec) where

import Day04
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Time

expectedShifts :: [Shift]
expectedShifts = [
  Shift 10 (fromGregorian 1518 11 01) [(05, 25), (30, 55)],
  Shift 99 (fromGregorian 1518 11 02) [(40, 50)],
  Shift 10 (fromGregorian 1518 11 03) [(24, 29)],
  Shift 99 (fromGregorian 1518 11 04) [(36, 46)],
  Shift 99 (fromGregorian 1518 11 05) [(45, 55)]
  ]

spec :: Spec
spec = do
    describe "getShifts" $
      it "should work with basic examples" $ do
        input <- readFile "input/04.test.txt"
        getShifts input `shouldBe` expectedShifts
