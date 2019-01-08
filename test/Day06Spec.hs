module Day06Spec (spec) where

import Day06
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do {

  ; describe "dist" $ do {

      ; it "is commutative" $ property $
          \(a, b, c, d) -> dist (Pt a b) (Pt c d) == dist (Pt c d) (Pt a b)

      ; it "should work with basic examples" $ do {
          ; map (uncurry dist)
            [(Pt 1 1, Pt 1 2), (Pt 1 1, Pt 1 5), (Pt 3 4, Pt 5 5)]
            `shouldBe` [1, 4, 3]
          }
      }

  ; describe "day06a" $ do {
      ; it "should work with basic examples" $ do {
          ; input <- readFile "input/06.test.txt"
          ; day06a input `shouldBe` 17
          }
      }

  }
