{-# LANGUAGE TemplateHaskell #-}
module Subsets where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

perms [] = [[]]
perms xs = concatMap (\x -> (x:) <$> perms (delete x xs)) xs

kSubsets _ 0 = [[]]
kSubsets [] _ = []
kSubsets (x:xs) k = withHead ++ withoutHead
  where withHead    = fmap (x:) (kSubsets xs (k-1))
        withoutHead = kSubsets xs k

subsetsCardinality :: [a] -> Int -> Int
subsetsCardinality xs k
  | length xs < k = 0
  | otherwise =
  let n = genericLength xs in
  let fact m = product [1..m] in
    (fact n) `div` ((fact k) * (fact $ n - k))

prop_kSubsets :: Positive Int -> Positive Int -> Bool
prop_kSubsets (Positive n) (Positive k) = actual == expected
  where
    n' = n `mod` 20
    k' = k `mod` 20
    actual = genericLength $ kSubsets [1..n'] k'
    expected = subsetsCardinality [1..n'] k'

return []
main = ($verboseCheckAll)
