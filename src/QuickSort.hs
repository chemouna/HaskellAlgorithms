module QuickSort where

import Data.List (partition)

quickSort1 [] = []
quickSort1 (x:xs) = quickSort1 [y | y <- xs, y < x] ++ [x] ++ quickSort1 [y | y <- xs, y > x]

quickSort2 [] = []
quickSort2 (x:xs) = quickSort2 ys ++ [x] ++ quickSort2 zs
    where (ys, zs) = partition (< x) xs