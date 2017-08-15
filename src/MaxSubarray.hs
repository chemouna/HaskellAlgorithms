module MaxSubarray where

import Control.Lens
import Test.Hspec
import Test.QuickCheck


kadane :: [Integer] -> (Integer, Int, Int)
kadane xs = kadaneHelper xs 0 0 0 0 0 0
  where kadaneHelper [] maxSum _ _ fStart fEnd _ = (maxSum, fStart, fEnd)
        kadaneHelper (y:ys) maxSum maxStart maxEnd fStart fEnd currSum =
          case (currSum + y > maxSum) of
            True -> kadaneHelper ys (currSum + y) maxStart (maxEnd + 1) maxStart (maxEnd + 1) (currSum + y)
            _ -> case (currSum + y < 0) of
                   True -> kadaneHelper ys maxSum (maxEnd + 1) (maxEnd + 1) fStart fEnd 0
                   _    -> kadaneHelper ys maxSum maxStart maxEnd fStart fEnd (currSum + y)

kadane2 :: [Integer] -> (Integer, Int, Int)
kadane2 xs = kadaneHelper2 xs 0 0 0 0 0 0
  where kadaneHelper2 [] maxSum _ _ fStart fEnd _ = (maxSum, fStart, fEnd)
        kadaneHelper2 (y:ys) maxSum maxStart maxEnd fStart fEnd currSum =
          if (currSum + y > maxSum) then
            kadaneHelper2 ys (currSum + y) maxStart (maxEnd + 1) maxStart (maxEnd + 1) (currSum + y)
            else
                 if (currSum + y < 0) then kadaneHelper2 ys maxSum (maxEnd + 1) (maxEnd + 1) fStart fEnd 0
                 else kadaneHelper2 ys maxSum maxStart maxEnd fStart fEnd (currSum + y)



main = hspec $ do
  describe "kadane" $ do
    it "should work for the empty list" $
      (kadane [] ^. _1) `shouldBe` 0
    
    it "should work for the example" $
      (kadane [-2, 1, -3, 4, -1, 2, 1, -5, 4] ^. _1) `shouldBe` 6

    it "should return the sum for all positive lists" $
      property $ forAll (listOf $ arbitrary `suchThat` (>= 0)) $ \xs ->
        (kadane xs ^. _1) `shouldBe` sum xs

    it "should return zero for negative lists" $
      property $ forAll (listOf $ arbitrary `suchThat` (< 0)) $ \xs ->
        (kadane xs ^. _1) `shouldBe` 0

