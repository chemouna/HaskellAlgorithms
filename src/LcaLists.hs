module LcaLists where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

lca :: Eq a => [a] -> [a] -> Maybe a
lca xs ys = let (m, n) = (length xs, length ys) in
  if m < n then match xs $ drop (n - m) ys
  else match (drop (m - n) xs) ys

match :: Eq a => [a] -> [a] -> Maybe a
match [] _ = Nothing
match _ [] = Nothing
match (x:xs) (y:ys)
  | x == y = Just x
  | otherwise = match xs ys

prop_lca :: [Int] -> Int -> Int -> Bool
prop_lca xs n m = lca ys zs == if suffix == [] then Nothing else Just (head suffix)
    where
      xs' = nub (0:xs)
      n' = (abs n) `mod` (length xs')
      (asbs, suffix) = splitAt (1 + n') xs'
      m' = (abs m) `mod` (length asbs)
      (as, bs) = splitAt m' asbs
      (ys, zs) = (as ++ suffix, bs ++ suffix)

