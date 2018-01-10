

module Cf112APetyaAndStrings where

import Data.List
import Data.Char

solve :: String -> String -> Int
solve [] [] = 0
solve _ [] = 1
solve [] _ = -1
solve (x:xs) (y:ys) = go (map toLower (x:xs)) (map toLower (y:ys))
  where go (x:xs) (y:ys)
          | x < y = -1
          | x > y = 1
          | x == y = solve xs ys

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  print $ solve a b

