
module NonAdjacentSum where

-- recursive repetitve solution (slow as it does a lot of calculations mutliple times)
maxsum :: Num a => [a] -> a
maxsum [] = 0
maxsum [x] = x
maxsum (x:y:xs) = (x + maxsum xs) + maxsum (y:xs)

