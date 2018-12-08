module Day02Spec (spec) where

import Day02
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  let expected = [
             ("abcdee", (False, True)),
             ("bababc", (True, True)),
             ("abbcde", (True, False)),
             ("abcccd", (False, True)),
             ("aabcdd", (True, False))
             ] in do
  describe "hasDuo" $
    it "should work with given examples" $ do
      map hasDuo (map fst expected) `shouldBe` map (fst . snd) expected

  describe "hasTrio" $
    it "should work with given examples" $ do
      map hasTrio (map fst expected) `shouldBe` map (snd . snd) expected
