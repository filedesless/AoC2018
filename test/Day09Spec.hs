module Day09Spec (spec) where

import Day09
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Deque

spec :: Spec
spec = do {

  ; describe "turn" $ do {
      ; it "should work with basic examples" $ do {
          ; getScores (turn game 9 25) `shouldBe` IntMap.fromList [(5, 32)]
          }
      }

  ; describe "highest" $ do {
      ; it "should return 8317, given 10 players; last marble is worth 1618 points" $ do {
          ; highest 10 1618 `shouldBe` 8317
          }

      ; it "should return 146373, given 13 players; last marble is worth 7999 points" $ do {
          ; highest 13 7999 `shouldBe` 146373
          }

      ; it "should return 2764, given 17 players; last marble is worth 1104 points" $ do {
          ; highest 17 1104 `shouldBe` 2764
          }

      ; it "should return 54718, given 21 players; last marble is worth 6111 points" $ do {
          ; highest 21 6111 `shouldBe` 54718
          }

      ; it "should return 37305, given 30 players; last marble is worth 5807 points" $ do {
          ; highest 30 5807 `shouldBe` 37305
          }
      }
  }
