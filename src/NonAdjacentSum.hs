

module NonAdjacentSum where

-- recursive repetitve solution (slow as it does a lot of calculations mutliple times)
maxsum [] = 0
maxsum [x] = x
maxsum (x:y:xs) = max (x + maxsum xs) (maxsum (y:xs))

-- iterative solution using foldl
maxsum_iter xs = max a b where
  (a, b) = foldl (\(i, e) x -> (x + e, max i e)) (0, 0) xs

-- lazy infinite recursive solution
maxsums_lazy [] = []
maxsums_lazy [x] = [0, x]
maxsums_lazy (x:xs) = ms where
  ms = 0 : x : zipWith3 (\x' m m' -> max (x' + m) m') xs ms (tail ms)
