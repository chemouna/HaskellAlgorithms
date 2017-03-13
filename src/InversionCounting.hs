module InversionCounting where

merge :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
merge left [] counter leftLength =  (left, counter, leftLength)
merge [] right counter _ = (right, counter, 0)
merge (l:ls) (r:rs) counter leftLength
  | l <= r = (l:left, leftCount, leftLength')
  | otherwise = (r:right, rightCount, leftLength'')
  where
    counter' = counter + leftLength
    (left, leftCount, leftLength') = merge ls (r:rs) counter (leftLength - 1)
    (right, rightCount, leftLength'') = merge (l:ls) rs counter' leftLength


inversionCount :: [Int] -> Int -> ([Int], Int)
inversionCount [] _ = ([], 0)
inversionCount [x] _ = ([x], 1)
inversionCount xs len =
  let (left, leftCount) = inversionCount leftList leftLength
      (right, rightCount) = inversionCount rightList rightLength in
      let (cross, crossCount, _) = merge left right (leftCount + rightCount) leftLength in
          (cross, crossCount)
  where
    leftLength = len `div` 2
    rightLength = len - leftLength
    (leftList, rightList) = splitAt leftLength xs
