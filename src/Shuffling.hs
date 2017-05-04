
module Shuffling where

import System.Random
import Data.Array.IO
import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import Data.Array.ST
import GHC.Arr

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n = newListArray (1, n)


shuffle' :: RandomGen g => [a] -> Rand g [a]
shuffle' xs = do
  let n = length xs
  rands <- take n `fmap` getRandomRs (0, n-1)
  let ar = runSTArray $ do
      ar <- thawSTArray $ listArray (0, n-1) xs
      forM_ (zip [0..(n-1)] rands) $ \(i, j) -> do
         vi <- readSTArray ar i
         vj <- readSTArray ar j
         writeSTArray ar j vi
         writeSTArray ar i vj
      return ar
  return (elems ar)

