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

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ i:j | i <- xs, j <- perms $ delete i xs]

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = and [x `elem` ys && index x xs /= index x ys | x <- xs]
  where
    index v (x:xs) | v == x = 0
                   | otherwise = 1 + index v xs

derangements :: Eq a => [a] -> [[a]]
derangements xs = filter (\p -> isDerangement p xs) (perms xs)

prop_isPerm :: Ord a => [a] -> Bool
prop_isPerm ls = isPermutation ls (sort ls) == True

return []
main = ($verboseCheckAll)
