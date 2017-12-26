
module Addrev where

main = do
  input <- getLine
  input <- getContents
  mapM (putStrLn . show . reverseNumber . sumReverse . words) (lines input)

parseNumber n = read n :: Int
reverseNumber = parseNumber . reverse . show
sumReverse a = sum (map (parseNumber . reverse) a)
