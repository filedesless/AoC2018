module Day07Spec (spec) where

import Day07
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do {

  ; describe "day07a" $ do {

      ; it "should work with basic examples" $ do {
          ; input <- readFile "input/07.test.txt"
          ; day07a input `shouldBe` "CABDFE"
          }
      }

  }
