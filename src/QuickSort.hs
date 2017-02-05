{-# LANGUAGE TemplateHaskell #-}

module QuickSort (
                 quickSort1,
                 quickSort2
                 ) where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

quickSort1 [] = []
quickSort1 (x:xs) = quickSort1 [y | y <- xs, y < x] ++ [x] ++ quickSort1 [y | y <- xs, y > x]

quickSort2 [] = []
quickSort2 (x:xs) = quickSort2 ys ++ [x] ++ quickSort2 zs
    where (ys, zs) = partition (< x) xs

prop_quicksort_idemp :: [Int] -> Bool
prop_quicksort_idemp xs = quickSort1 (quickSort1 xs) == quickSort1 xs

prop_quicksort_nn_min xs = not (null xs) ==> head (quickSort1 xs) == minimum xs

prop_quicksort_nn_max xs = not (null xs) ==> head (reverse (quickSort1 xs)) == maximum xs

prop_quicksort_sort xs = quickSort1 xs == sort xs

prop_quicksort_sort_fix xs = quickSort1 xs == nub (sort xs)

isDistinct (x:xs) = not (x `elem` xs) && isDistinct xs
isDistinct _      = True

prop_qsort_distinct :: [Int] -> Bool
prop_qsort_distinct = isDistinct . quickSort1

prop_quicksort_sort_distinct xs = isDistinct xs ==> quickSort1 xs == sort xs

return []
main = $quickCheckAll
