module GraphCommon where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Foldable (foldl')
import Control.Monad

type Graph = IntMap [Int]

readListIO :: Read a => IO [a]
readListIO = mapM readIO . words =<< getLine

readDirectedGraph :: IO Graph
readDirectedGraph = do
  [vertices, edgesCount] <- readListIO
  edges <- replicateM edgesCount readListIO
  let g0 = IntMap.fromList [(i, []) | i <- [1..vertices]]
      addEdge x y = IntMap.insertWith (++) x [y]
  return (foldl' (\g [x,y] -> addEdge x y g) g0 edges)

