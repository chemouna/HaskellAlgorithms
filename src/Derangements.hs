
module Derangements where

import Data.List
import Control.Monad

derangements :: Eq a => [a] -> [[a]]
derangements = (\x -> filter (and . zipWith (/=) x)) <*> permutations

subfactorial :: (Eq a, Num a) => a -> a
subfactorial 0 = 1
subfactorial 1 = 0
subfactorial n = (n - 1) * (subfactorial (n - 1) + subfactorial (n - 2))

main :: IO ()
main = do
  print $ derangements [1..5]
  putStrLn " "
  -- print the count of derangements of each number and its subfactorial
  forM_ [1..9] $
    \i -> putStrLn $ mconcat [(show (length (derangements [1..i]))), " ", show (subfactorial i)]

  putStrLn " "
  print $ subfactorial 30


