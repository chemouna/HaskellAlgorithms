
module LcaLists where

lca :: Eq a => [a] -> [a] -> Maybe a
lca xs ys = let (m, n) = (length xs, length ys) in
  if m < n then match xs (drop (n - m) ys)
  else match (drop (n - m) xs) ys

match :: Eq a => [a] -> [a] -> Maybe a
match [] _ = Nothing
match _ [] = Nothing
match (x:xs) (y:ys) 
  | x == y = Just x
  | otherwise = match xs ys
