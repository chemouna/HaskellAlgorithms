
module PascalTriangle where

import Text.Printf

pascal :: [[Integer]]
pascal = iterate (\prev -> 1 : zipWith (+) prev (tail prev) ++ [1]) [1]

next row = zipWith (+) (row ++ [0]) ([0] ++ row)
pascal2 = iterate next [1]

pascal3 = [1] : map (zipWith (+) <$> ([0] ++) <*> (++ [0])) pascal3

prettyPascal :: Int -> IO ()
prettyPascal n = mapM_ (\r -> printf "%*s\n" (div (longest + length r) 2) r) rows
    where rows = map (unwords . map show) $ take (n + 1) pascal
          longest = length $ last rows
