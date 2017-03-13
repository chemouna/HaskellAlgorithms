module InversionCounting where

merge :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
merge left [] counter left_length       = (left, counter, left_length)
merge [] right counter _      = (right, counter, 0)
merge (l:ls) (r:rs) counter left_length
  | l <= r = (l:left, left_count, left_length')
  | otherwise = (r:right, right_count, left_length'')
  where
    counter' = counter + left_length
    (left, left_count, left_length')    = merge ls (r:rs) counter (left_length - 1)
    (right, right_count, left_length'') = merge (l:ls) rs counter' left_length


inversionCount :: [Int] -> Int -> ([Int], Int)
inversionCount [] _         = ([], 0)
inversionCount [x] _     = ([x], 0)
inversionCount list length' =
  let (left, left_count)   = inversionCount left_list left_length
      (right, right_count) = inversionCount right_list right_length in
    let (cross, cross_count, _) = merge left right (left_count + right_count) left_length in
        (cross, cross_count)
  where
    left_length  = length' `div` 2
    right_length = length' - left_length
    (left_list, right_list) = splitAt left_length list


-- Solution 2
splitInHalf :: Ord a => [a] -> ([a], [a])
splitInHalf xs = (take n xs, drop n xs)
  where n = length xs `div` 2

mergeandcount :: Ord a => [a] -> [a] -> ([a], Int)
mergeandcount [] rs = (rs, 0)
mergeandcount ls [] = (ls, 0)
mergeandcount ls@(l:lst) rs@(r:rst) =
  if l < r then let (ds, dc) = mergeandcount lst rs in (l:ds, dc)
           else let (ds, dc) = mergeandcount ls rst in (r:ds, dc + length ls)

sortandcount :: Ord a => [a] -> ([a], Int)
sortandcount [] = ([], 0)
sortandcount a@([_]) = (a, 0)
sortandcount xs = (xs', lc + rc + xc)
  where
     (ls, rs) = splitInHalf xs
     (ls', lc) = sortandcount ls
     (rs', rc) = sortandcount rs
     (xs', xc) = mergeandcount ls' rs'

inversionCount2 :: Ord a => [a] -> Int
inversionCount2 = snd . sortandcount
