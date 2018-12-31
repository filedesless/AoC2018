module Day06Spec (spec) where

import Day06
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do {

  ; describe "day06a" $ do {
      ; it "should work with basic examples" $ do {
          ; input <- readFile "input/06.test.txt"
          ; day06a input `shouldBe` 17
          }
      }

  }
