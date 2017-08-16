{-# LANGUAGE FlexibleContexts #-}
module MaxSubarray where

import Control.Lens
import Test.Hspec
import Test.QuickCheck
import Debug.Trace
import Data.List (maximumBy, foldl')
import Data.Monoid

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
          (currSum + y > maxSum) = trace "1" helper ys (currSum + y) maxStart e (currSum + y)
          | (currSum + y < 0 && currSum < 0) = trace "2.1" helper ys 0 0 0 0
          | (currSum + y < 0 && currSum >= 0) = trace "2.2" helper ys maxSum (maxStart + 1) (maxEnd + 1) 0
          | otherwise = trace "3" helper ys maxSum maxStart maxEnd (currSum + y)
          where e = min (length xs - 1) (maxEnd + 1)

--

data Range a = Range !a !Int !Int deriving (Eq)
rangeVal (Range x _ _) = x

instance (Show a) => Show (Range a) where
    show (Range x s e) = "{" ++ show x ++ " [" ++ show s ++ "-" ++ show e ++ "]}"

instance (Ord a) => Ord (Range a) where
    compare (Range x1 s1 e1) (Range x2 s2 e2) = compare x1 x2 <> (flip compare) (e1 - s1) (e2 - s2)

annotate :: [a] -> [Range a]
annotate = zipWith (\n x -> Range x n n) [0..]

merge :: (Num a) => Range a -> Range a -> Range a
merge (Range x1 s _) (Range x2 _ e) = Range (x1 + x2) s e

findMax :: (Num a, Ord a) => [a] -> Maybe (Range a)
findMax [] = Nothing
findMax xs = let (y:ys) = annotate xs in Just $ snd (foldl' go (y, y) ys)
  where go (max_ending_here, max_so_far) r = let max_ending_here' = max r (merge max_ending_here r)
                                                 max_so_far' = max max_so_far max_ending_here'
                                             in (max_ending_here', max_so_far')

findMaxSum :: (Num a, Ord a) => [a] -> Maybe a
findMaxSum = (fmap rangeVal) . findMax

prop_largest :: [Integer] -> Property
prop_largest xs = not (null xs) ==> let (Just v) = findMaxSum xs in all (<= v) xs

prop_correctRange :: [Integer] -> Property
prop_correctRange xs = not (null xs) ==> sum sublist == val
  where Just (Range val start end) = findMax xs
        sublist = take (end - start + 1) (drop start xs)

--

main = hspec $ do
  describe "kadane" $ do
    it "should work for the empty list" $
      kadane [] `shouldBe` (0, 0, 0)

    it "should work for the example" $
      (kadane [-2, 1, -3, 4, -1, 2, 1, -5, 4]) `shouldBe` (6, 3, 6)

    it "should work for the example [-2, 1] " $
      (kadane [-2, 1]) `shouldBe` (1, 1, 1)

    it "should return zero for negative lists" $
      property $ forAll (listOf $ arbitrary `suchThat` (< 0)) $ \xs ->
        kadane xs `shouldBe` (0,0,0)

    it "should return the sum for all positive lists" $
      property $ forAll (listOf $ arbitrary `suchThat` (>= 0)) $ \xs ->
        kadane xs `shouldBe` (sum xs, 0, max 0 (length xs - 1))
