{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import QuickSort (quickSort1, quickSort2)


prop_quicksort_idemp :: [Int] -> Bool
prop_quicksort_idemp xs = quickSort1 (quickSort1 xs) == quickSort1 xs


return []
main = $quickCheckAll

