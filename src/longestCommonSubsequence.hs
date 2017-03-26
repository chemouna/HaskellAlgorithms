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
lcs_dp' :: Ord a => [a] -> [a] -> Int -> Int -> Array (Int, Int) Int
lcs_dp' s t m n =
        a where
                a = array ((0, 0), (m, n))
                        ([((0,j), 0) | j <- [0..n]] ++
                         [((i,0), 0) | i <- [0..m]] ++
                         [((i,j),
                                if ((s !! (i-1)) == (t !! (j-1)))
                                then (a!(i-1,j-1)) + 1
                                else max (a!(i-1,j)) (a!(i, j-1)))
                                | i <- [1..m], j <- [1..n]])

lcs_dp :: Ord a => [a] -> [a] -> Array (Int, Int) Int
lcs_dp s t = lcs_dp' s t (length s) (length t)
