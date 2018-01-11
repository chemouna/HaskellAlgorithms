module Cf6BPresidentOffice where

import Data.List
import Control.Monad
import Data.Ord
import Data.Maybe

{--
i need to count adjacencies left and right of his color
so first i need to locate the president color

shld be instead adjacencies of all matches of its color and take max

-}

solve :: String -> [String] -> Int
solve s m = length $ filter (\x -> (x /= '.') && (x /= c)) $ map (\(x, y) -> (m !! x) !! y) is 
  where
    -- TODO: handle boundaries
    is = [ ((fst ip) - 1, snd ip), (fst ip, (snd ip) - 1), ((fst ip) + 1, snd ip), (fst ip, (snd ip) + 1) ]
    ip = indexPresident m c 
    vs =  map (\x -> read x :: Integer) (init l)
    c = head (last l)
    l = words s

-- ["G.B.",".RR.","TTT."]
-- lets for now get just the furthest index and if that doest work get all and find max

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
  inputs <- replicateM 3 getLine
  print $ solve input inputs

  -- solve

  -- s <- getLine
  -- l <- words s
  --vs <- map (\x -> read x :: Integer) (init l)
  -- c <- last l
  -- putStrLn $  "test" -- l
