
module Cf6BPresidentOffice where

import Data.List
import Control.Monad

solve :: String -> [String] -> ([Integer], String)
solve s m = (vs, c)
  where
    vs =  map (\x -> read x :: Integer) (init l)
    c = last l
    l = words s


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


