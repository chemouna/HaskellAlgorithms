module QuickSelect where


import Data.List (partition)

quickSelect :: Ord a => Int -> [a] -> a
quickSelect k (x:xs) | k < l = quickSelect k ys
                     | k > l = quickSelect (k - l - 1) zs
                     | otherwise = x
         where (ys, zs) = partition (< x) xs
               l = length ys


-- Example : show the first, second, third, ... up to the tenth largest member of
-- a vector [9, 8, 7, 6, 5, 0, 1, 2, 3, 4] -> kind of a sort
main :: IO ()
main = do
   let xs = [9, 8, 7, 6, 5, 0, 1, 2, 3, 4]
   print $ map (\i -> quickSelect i xs) [0 ..length xs - 1]


