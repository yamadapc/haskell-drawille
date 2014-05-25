module DrawilleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M

import qualified System.Drawille as D

main :: IO ()
main = hspec spec

-- ---------------
-- | 0x01 | 0x08 |
-- | 0x02 | 0x10 |
-- | 0x04 | 0x20 |
-- | 0x40 | 0x80 |
-- ---------------

spec :: Spec
spec = do
  describe "empty" $
    it "is an empty map of points to ints" $
      D.empty `shouldBe` M.empty

  describe "set" $ do
    it "sets nonexistent coordinates" $
      M.lookup (0, 0) (D.set D.empty (1, 1)) `shouldBe` Just 16

    it "updates existent coordinates" $ do
      let c = D.set D.empty (1, 1)
      M.lookup (0, 0) (D.set c (0, 1)) `shouldBe` Just 18

  describe "unset" $ do
    it "clears existent coordinates" $ do
      let c = D.set D.empty (1, 1)
      M.lookup (0, 0) (D.unset c (1, 1)) `shouldBe` Just 0

    it "updates special cases" $ do
      let c = D.set (D.set D.empty (1, 1)) (0, 0)
      M.lookup (0, 0) (D.unset c (1, 1)) `shouldBe` Just 1

    it "reverses the `set` operations" $ property unsetSetProp

  describe "toggle" $ do
    it "sets unset coordinates" $ do
      let c = D.set D.empty (1, 1)
      M.lookup (0, 0) (D.toggle c (0, 0)) `shouldBe` Just 17

    it "unsets set coordinates" $ do
      let c = D.set D.empty (1, 1)
      M.lookup (0, 0) (D.toggle c (1, 1)) `shouldBe` Just 0

    it "reverses itself" $ property toggleRevProp

  describe "get" $
    it "gets a coordinates' value" $ do
      let c = D.set D.empty (1, 1)
      D.get c (1, 1) `shouldBe` True
      D.get c (0, 0) `shouldBe` False

  describe "frame" $
    it "gives back a string representation of canvas" $ do
      let c = D.set (D.set D.empty (0, 0)) (1, 2)
      D.frame c `shouldBe` "⠡\n"
      let d = D.fromList [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5)]
      D.frame d `shouldBe` "⡇\n⠃\n"

  describe "fromList" $
    it "creates a canvas based on a list of tuples" $ do
      let c = D.set (D.set D.empty (0, 0)) (10, 20)
      D.fromList [(0, 0), (10, 20)] `shouldBe` c

toggleRevProp :: Positive Int -> Positive Int -> Bool
toggleRevProp (Positive x) (Positive y) = M.lookup (D.toPs p) c == Just 0
    where p = (x, y)
          c = D.toggle (D.toggle D.empty p) p

unsetSetProp :: Positive Int -> Positive Int -> Bool
unsetSetProp (Positive x) (Positive y) = M.lookup (D.toPs p) c == Just 0
    where p = (x, y)
          c = D.unset (D.set D.empty p) p
