module MaxSubarray where

import Control.Lens
import Test.Hspec
import Test.QuickCheck

kadane1 :: [Integer] -> (Integer, Int, Int)
kadane1 xs = kadaneHelper xs 0 0 0 0 0 0
  where kadaneHelper [] maxSum _ _ fStart fEnd _ = (maxSum, fStart, fEnd)
        kadaneHelper (y:ys) maxSum maxStart maxEnd fStart fEnd currSum =
          case (currSum + y > maxSum) of
            True -> kadaneHelper ys (currSum + y) maxStart end maxStart end (currSum + y)
                    where end = (min (length xs - 1) (maxEnd + 1))
            _ -> case (currSum + y < 0) of
                   True -> kadaneHelper ys maxSum end' (min (length xs - 1) (maxEnd + 1)) fStart fEnd 0
                           where end' = (min (length xs - 1) (maxEnd + 1))
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

kadane :: [Integer] -> (Integer, Int, Int)
kadane xs = helper xs 0 0 0 0
  where helper [] maxSum maxStart maxEnd _ = (maxSum, maxStart, maxEnd)
        helper (y:ys) maxSum maxStart maxEnd currSum |
          (currSum + y > maxSum) = helper ys (currSum + y) maxStart (maxEnd + 1) (currSum + y)
          | (currSum + y < 0) = helper ys maxSum (maxEnd + 1) (maxEnd + 1) 0
          | otherwise = helper ys maxSum maxStart maxEnd (currSum + y)

main = hspec $ do
  describe "kadane" $ do
    it "should work for the empty list" $
      kadane [] `shouldBe` (0, 0, 0)

    it "should work for the example" $
      (kadane [-2, 1, -3, 4, -1, 2, 1, -5, 4]) `shouldBe` (6, 3, 6)

    it "should return zero for negative lists" $
      property $ forAll (listOf $ arbitrary `suchThat` (< 0)) $ \xs ->
        kadane xs `shouldBe` (0,0,0)

    it "should return the sum for all positive lists" $
      property $ forAll (listOf $ arbitrary `suchThat` (>= 0)) $ \xs ->
        kadane xs `shouldBe` (sum xs, 0, max 0 (length xs - 1))
