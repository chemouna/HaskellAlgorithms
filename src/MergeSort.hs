module MergeSort where

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (as, bs) = split xs
  in merge (mergeSort as) (mergeSort bs)


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys


split :: (Ord a) => [a] -> ([a], [a])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)
split [x] = ([x], [])
split [] = ([], [])

mergeSort2 :: Ord a => [a] -> [a]
mergeSort2 xs
  | length xs > 1 = merge (mergeSort2 ls) (mergeSort2 rs)
  | otherwise = xs
  where (ls, rs) = splitInHalf xs

splitInHalf :: Ord a => [a] -> ([a], [a])
splitInHalf xs = (take n xs, drop n xs)
  where n = length xs `div` 2
