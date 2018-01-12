module Cf6BPresidentOffice where

import Data.List
import Control.Monad
import Data.Ord
import Data.Maybe

solve :: String -> [String] -> Int -- [(Int, Int)] -- Int
solve s mat = length $ filter (\x -> (x /= '.') && (x /= c)) $ map (\(x, y) -> (mat !! x) !! y) is
  where
    is = [
            (max ((fst ip) - 1) 0, min (snd ip) (m - 1)),
            (min (fst ip) (n - 1), max ((snd ip) - 1) 0),
            (min ((fst ip) + 1) (n - 1), min (snd ip) (m - 1)),
            (min (fst ip) (n - 1), min ((snd ip) + 1) (m - 1))
         ]
    ip = indexPresident mat c
    vs = map (\x -> read x :: Integer) (init l)
    c = head (last l)
    n = read (head l) :: Int
    m = read (l !! 1) :: Int
    l = words s

indexPresident :: [String] -> Char -> (Int, Int)
indexPresident m c = head $ map (\(x, y) -> (y, fromJust x))
  $ filter (\(x, y) -> isJust x) (map (\(x, y) -> (findMaxIndexList x c, y)) (zip m [0..]))


findMaxIndexList :: String -> Char -> Maybe Int
findMaxIndexList s v = if null l then Nothing
  else Just (snd (last l))
  where
    l = filter (\(x,y) -> x == v) (zip s [0..])

main :: IO ()
main = do
  input <- getLine
  inputs <- replicateM (read (head (words input)) :: Int) getLine
  print $ solve input inputs

