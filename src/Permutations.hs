{-# LANGUAGE TemplateHaskell #-}

module Permutations where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs (y:ys)
  | length xs /= length (y:ys) = False
  | otherwise = isPermutation (delete y xs) ys

prop_perm :: Ord a => [a] -> Bool
prop_perm ls = isPermutation ls (sort ls) == True


return []
main = ($verboseCheckAll)
