{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module LongestCommonSubsequence where

import qualified Data.MemoCombinators as M
import Data.Array

longest xs ys = if length xs > length ys then xs else ys

lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys)
  | x == y = x : lcs xs ys
  | otherwise = longest (lcs (x:xs) ys) (lcs xs (y:ys))


-- Memoized version
memoize = M.memo2 mString mString
mString = M.list M.char

lcsm = memoize lcs

-- DP version
lcs_dp :: [Char] -> [Char] -> Array (Int, Int) Int
lcs_dp xs ys = a!(0,0) where
  n = length xs
  m = length ys
  a = array ((0, 0), (n, m)) $ l1 ++ l2 ++ l3
  l1 = [((i,0), 0) | i <- [0..n]]
  l2 = [((0, j), 0) | j <- [0..m]]
  l3 = [((i, j),
        if (xs ! (i-1)) == (ys ! (j-1))
        then (a ! (i-1, j-1)) + 1
        else max (a ! (i - 1, j)) (a ! (i, j - 1)))
        | i <- [1..m], j <- [1..n]]
