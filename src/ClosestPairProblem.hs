
module ClosestPairProblem where

import Test.QuickCheck

type Point = (Double, Double)

-- brute force approach
closestPairBruteForce :: [Point] -> Maybe (Point, Point)
closestPairBruteForce pairs = snd $ foldl (\ acc@(minp, _) (p1, p2) ->
                                             if distance p1 p2 < minp
                                                then (distance p1 p2, Just(p1, p2))
                                                else acc)
                                          (fromIntegral (maxBound :: Int), Nothing)
                                          [(pairs !! i, pairs !! j) |
                                            i <- [0..(length pairs - 1)],
                                            j <- [0..(length pairs - 1)],
                                            i /= j]
                              where distance p1@(x1, y1) p2@(x2, y2) = sqrt $
                                      ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)


-- property tests
-- TODO: write test properties 
f_closest :: ([Point] -> (Point, Point)) -> [Point] -> Property
f_closest = undefined
