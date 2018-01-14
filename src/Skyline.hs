
module Skyline where

import Data.List

-- Solution 1: By Calculating the height of the  skyline at all endpoints
skyline1 bs = sort (foldl add_endpoints [] bs)
  where
    add_endpoints xs (x1, h, x2) = xs ++ [(x1, height x1), (x2, height x2)]
    height x = maximum (0 : [h | (x1, h, x2) <- bs, x >= x1 && x < x2])

-- height x calculates the highest height for coordinates interleaving with x

-- Solution 2: by iteratively adding buildings
skyline bs = sort (foldl add_building [] (sortWith (\(_,h,_)->h) bs))
             where
                height xs y =
                  (snd . maximum) ((0, 0): [(x, h)| (x, h) <- xs, x <= y])
                add_building xs (x1, h, x2) =
                  [(x, h)| (x, h) <- xs, x < x1 || x > x2] ++
                  [(x1, h), (x2, height xs x2)]

