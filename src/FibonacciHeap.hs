
module FibonacciHeap where

data Node a = Node {value :: a, parent :: Maybe(Node a), children :: [Node a],
                    degree :: Int, isMarked :: Bool}
  deriving Show

