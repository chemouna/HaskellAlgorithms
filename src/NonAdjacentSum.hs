
module NonAdjacentSum where

-- recursive repetitve solution (slow as it does a lot of calculations mutliple times)
maxsum [] = 0
maxsum [x] = x
maxsum (x:y:xs) = max (x + maxsum xs) (maxsum (y:xs))

-- iterative solution using foldl
maxsum_iter xs = max a b where
  (a, b) = foldl (\(i, e) x -> (x + e, max i e)) (0, 0) xs
