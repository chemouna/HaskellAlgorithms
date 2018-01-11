module Cf236ABoyOrGirl where

import Data.List

solve :: String -> String
solve s
  | v == 0 = "CHAT WITH HER!"
  | otherwise = "IGNORE HIM!"
  where v = length (nub s) `mod` 2


main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s
