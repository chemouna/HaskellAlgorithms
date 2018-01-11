module Cf6BPresidentOffice where

import Data.List
import Control.Monad
import Data.Ord

{--
i need to count adjacencies left and right of his color
so first i need to locate the president color

shld be instead adjacencies of all matches of its color and take max

-}

solve :: String -> [String] -> ([Integer], String)
solve s m = (vs, c)
  where
    vs =  map (\x -> read x :: Integer) (init l)
    c = last l
    l = words s

-- ["G.B.",".RR.","TTT."]
-- lets for now get just the furthest index and if that doest work get all and find max

findMaxIndexList :: String -> Char -> Int
findMaxIndexList s v = snd $ last $ filter (\(x,y) -> x == v) (zip s [0..])

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
