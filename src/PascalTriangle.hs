
module PascalTriangle where

pascal :: [[Integer]]
pascal = iterate (\prev -> 1 : zipWith (+) prev (tail prev) ++ [1]) [1]


next row = zipWith (+) (row ++ [0]) ([0] ++ row)
pascal2 = iterate next [1]

pascal3 = [1] : map (zipWith (+) <$> ([0] ++) <*> (++ [0])) pascal3
