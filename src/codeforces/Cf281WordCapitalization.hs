
module Cf281WordCapitalization where

import Data.Char

solve :: String -> String
solve [] = []
solve (x:xs) = toUpper x : xs

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s
