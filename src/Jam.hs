
module Jam (
  jam, jamOff, jamCases,
  gets, getsn
  --, getints, getintsn, getintegers, getdbls
) where

import Control.Monad
import Data.List

data Jam a = Jam (([String], [String]) -> (a, ([String], [String])))

instance Functor Jam where
  fmap = liftM

instance Applicative Jam where
  pure k = Jam (\s -> (k, s))
  (<*>) = ap

instance Monad Jam where
  Jam c1 >>= fc2 = Jam (\s0 -> let
    (r, s1) = c1 s0
    Jam c2 = fc2 r
    in c2 s1)
  return = pure

gets :: Jam String
gets = Jam (\(x:xs, ys) -> (x, (xs, ys++[x])))

getsn :: Int -> Jam [String]
getsn n = replicateM n gets

{--
getints :: Jam [Int]
getints = getem

getintegers :: Jam [Integer]
getintegers = getem

getintsn :: Int -> Jam [[Int]]
getintsn = getemn

getdbls :: Jam [Double]
getdbls = getem
--}

jamOff offset f = interact $ \s -> let
  Jam fun = f
  (_n:inp) = lines s
  n = read _n
  in unlines $ zipWith (++) (map (\k -> "Case #" ++ show (k+offset) ++ ": ") [1..n])
    $ unfoldr (\(xs, _) -> Just $ fun (xs, [])) (inp, [])

jam f = interact $ \s -> let
  Jam fun = f
  (_n:inp) = lines s
  n = read _n
  in unlines $ zipWith (++) (map (\k -> "Case #" ++ show k ++ ": ") [1..n])
    $ unfoldr (\(xs, _) -> Just $ fun (xs, [])) (inp, [])

jamCases idxs f = interact $ \s -> let
  Jam fun = f
  (_n:inp) = lines s
  n = read _n
  cases = take n $ unfoldr (\xy -> let
    (_, (xs, ys)) = fun xy in Just (ys, (xs, []))) (inp, [])
  in unlines $ (:) (show $ length idxs) $ concatMap ((cases!!) . (+ (-1))) idxs

  
