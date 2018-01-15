module SummerSellOff where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List

getInts :: IO [Int]
getInts = map (fst . fromJust . B.readInt) <$> B.words <$> B.getContents

parsePairs :: [Int] -> [(Int, Int)]
parsePairs [] = []
parsePairs (x:y:xs) = (x, y) : parsePairs xs

clamp u v w = max u (min v w)

solve :: Int -> [(Int, Int)] -> Integer
solve f xs = base + extra
  where base = sum . map toInteger $  [min k l | (k, l) <- xs]
        extra = sum . map toInteger . take f . reverse . sort $ [clamp 0 k (l - k) | (k, l) <- xs]

main :: IO ()
main = do
  _:f:xs <- getInts
  print $ solve f (parsePairs xs)
