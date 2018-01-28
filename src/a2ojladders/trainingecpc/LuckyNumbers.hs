
module LuckyNumbers where

next :: String -> String
next s
  | all (== '7') s  =  replicate (length s + 1) '4'
  | otherwise  =  reverse $ next' $ reverse s
  where
    next' ('4':s)  =  '7' : s
    next' ('7':s)  =  '4' : next' s
    next' _        =  "4"

lucky :: [String]
lucky = iterate next "4"

superLucky :: [Integer]
superLucky = map read $ filter isSuperLucky lucky
  where
    isSuperLucky x = length (filter (== '4') x) * 2 == length x

solve :: Integer -> Integer
solve n  =  head $ dropWhile (< n) superLucky

main :: IO ()
main  =  readLn >>= print . solve

