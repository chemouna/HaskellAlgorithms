
module SpojTest where

main :: IO ()
main = interact go
  where go = unlines . takeWhile (/= "42") . words
