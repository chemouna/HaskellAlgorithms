
module Plugin where

main = getLine >>= putStrLn . solve

solve = reverse . foldl f ""
f (y:ys) x | y == x = ys
f ys x = x:ys


