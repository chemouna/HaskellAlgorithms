
module Skyline where

import Data.List

skyline bs = sort (foldl add_endpoints [] bs)
  where
    add_endpoints xs (x1, h, x2) = xs ++ [(x1, height x1), (x2, height x2)]
    height x = maximum (0 : [h | (x1, h, x2) <- bs, x >= x1 && x < x2])
