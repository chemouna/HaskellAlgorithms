{-# LANGUAGE ScopedTypeVariables #-}
module FloodFill where

import Data.Array
import Data.List

data Colour = White | Black | Blue | Green | Red deriving (Show, Eq)

type Grid = Array (Int, Int) Colour
type Point = (Int, Int)

inBounds :: Grid -> Point -> Bool
inBounds grid (x, y) = x >= lowx && x <= highx && y >= lowy && y <= highy
  where ((lowx, lowy), (highx, highy)) = bounds grid

replace :: Grid -> Point -> Colour -> Grid
replace grid point replacement = if inBounds grid point then grid // [(point, replacement)] else grid

floodFill :: Grid -> Point -> Colour -> Colour -> Grid
floodFill grid point@(x, y) target replacement =
  if (grid ! (x, y) /= target) || not (inBounds grid point)
  then grid
  else gridNorth
       where
         grid' = replace grid point replacement
         gridEast = floodFill grid' (x + 1, y) target replacement
         gridWest = floodFill gridEast (x - 1, y) target replacement
         gridSouth = floodFill gridWest (x, y + 1) target replacement
         gridNorth = floodFill gridSouth (x, y - 1) target replacement


toComplexArray :: [[a]] -> Array (Int, Int) a
toComplexArray grid = array ((0,0),((length $ grid !! 0) - 1,(length grid) - 1))  entries
        where entries = concatMap (\z -> map (\y -> ((fst y, fst z), snd y))  (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) grid

printGrid :: Show a => Array (Int, Int) a ->  IO [()]
printGrid =  mapM (putStrLn . textRepresentation) . toSimpleArray

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
        where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation = unwords  . map show



