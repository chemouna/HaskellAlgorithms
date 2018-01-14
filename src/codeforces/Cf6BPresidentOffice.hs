module Cf6BPresidentOffice where

import Data.List
import Control.Monad
import Data.Ord
import Data.Maybe

solve :: String -> [String] -> Int -- [(Int, Int)] -- Int
solve s mat = maximum $ map total is
  where
    total xs = length $ filter (\x -> (x /= '.') && (x /= c)) $ map (\(x, y) -> (mat !! x) !! y) xs
    is = map (\(a,b) ->
                 [ (max (a - 1) 0, min b (m - 1)),
                   (min a (n - 1), max (b - 1) 0),
                   (min (a + 1) (n - 1), min b (m - 1)),
                   (min a (n - 1), min (b + 1) (m - 1)) ]) ip

    ip = indicesPresident mat c
    vs = map (\x -> read x :: Integer) (init l)
    c = head (last l)
    n = read (head l) :: Int
    m = read (l !! 1) :: Int
    l = words s


indicesPresident :: [String] -> Char -> [(Int, Int)]
indicesPresident m c = concatMap (\(x, ys) -> map (\v -> (x, v)) ys) xys
  where
    xys = filter ((> 0).length.snd) $  map (\(x, y) -> (y, findIndexList x c)) (zip m [0..])

findIndexList :: String -> Char -> [Int]
findIndexList s v = map snd $ filter ((== v).fst) (zip s [0..])

main :: IO ()
main = do
  input <- getLine
  inputs <- replicateM (read (head (words input)) :: Int) getLine
  print $ solve input inputs

import Data.List
import Control.Monad
import Data.Ord
import Data.Maybe

solve :: String -> [String] -> Int -- [(Int, Int)] -- Int
solve s mat = maximum $ map total is
  where
    total xs = length $ filter (\x -> (x /= '.') && (x /= c)) $ map (\(x, y) -> (mat !! x) !! y) xs
    is = map (\(a,b) ->
                 [ (max (a - 1) 0, min b (m - 1)),
                   (min a (n - 1), max (b - 1) 0),
                   (min (a + 1) (n - 1), min b (m - 1)),
                   (min a (n - 1), min (b + 1) (m - 1)) ]) ip

    ip = indicesPresident mat c
    vs = map (\x -> read x :: Integer) (init l)
    c = head (last l)
    n = read (head l) :: Int
    m = read (l !! 1) :: Int
    l = words s


indicesPresident :: [String] -> Char -> [(Int, Int)]
indicesPresident m c = concatMap (\(x, ys) -> map (\v -> (x, v)) ys) xys
  where
    xys = filter ((> 0).length.snd) $  map (\(x, y) -> (y, findIndexList x c)) (zip m [0..])

findIndexList :: String -> Char -> [Int]
findIndexList s v = map snd $ filter ((== v).fst) (zip s [0..])

main :: IO ()
main = do
  input <- getLine
  inputs <- replicateM (read (head (words input)) :: Int) getLine
  print $ solve input inputs

