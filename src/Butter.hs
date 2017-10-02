
module Butter (
  butter, butterCases,
  gets, getsn, getem, getemn, getints, getintsn, getintegers, getdbls
) where

import Control.Monad
import Data.List

data Butter a = Butter (([String], [String]) -> (a, ([String], [String])))

instance Functor Butter where
  fmap = liftM

instance Applicative Butter where
  pure k = Butter (\s -> (k, s))
  (<*>) = ap

instance Monad Butter where
  Butter c1 >>= fc2 = Butter (\s0 -> let
    (r, s1)   = c1 s0
    Butter c2 = fc2 r
    in c2 s1)
  return = pure

gets :: Butter String
gets = Butter (\(x:xs, ys) -> (x, (xs, ys++[x])))

getsn :: Int -> Butter [String]
getsn n = replicateM n gets

getem :: Read a => Butter [a]
getem = gets >>= return . map read . words

getemn :: Read a => Int -> Butter [[a]]
getemn n = getsn n >>= return . map (map read . words)

getints :: Butter [Int]
getints = getem

getintegers :: Butter [Integer]
getintegers = getem

getintsn :: Int -> Butter [[Int]]
getintsn = getemn

getdbls :: Butter [Double]
getdbls = getem

butter f = interact $ \s -> let
  Butter fun = f
  (_n:inp) = lines s
  n = read _n
  in unlines $ zipWith (++) (map (\k -> "Case #" ++ show k ++ ": ") [1..n])
    $ unfoldr (\(xs, _) -> Just . fun $ (xs, [])) $ (inp, [])

butterLn f = interact $ \s -> let
  Butter fun = f
  (_n:inp) = lines s
  n = read _n
  in unlines $ zipWith (++) (map (\k -> "Case #" ++ show k ++ ":\n") [1..n])
    $ unfoldr (\(xs, _) -> Just . fun $ (xs, [])) $ (inp, [])

butterCases idxs f = interact $ \s -> let
  Butter fun = f
  (_n:inp) = lines s
  n = read _n
  cases = take n $ unfoldr (\xy -> let
    (_, (xs, ys)) = fun xy in Just (ys, (xs, []))) (inp, [])
  in unlines $ (:) (show $ length idxs) $ concatMap (cases!!) idxs

  
