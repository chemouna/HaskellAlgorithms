
module SettlersTraining where

import Data.List

notOver k xs = any (< k) xs
levelUp = concatMap (\(x:xs) -> (x+1):xs) . group . sort
solve (k:xs) = length $ takeWhile (notOver k) $ iterate levelUp xs

main = interact $ show . solve . tail . map read . words
