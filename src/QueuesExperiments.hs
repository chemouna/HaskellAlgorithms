{-# LANGUAGE ScopedTypeVariables #-}

module QueuesExperiments where

class Queue q where
  empty :: q a
  head :: q a -> a
  tail :: q a -> q a
  snoc :: q a -> a -> q a

data Queue0 a = Q0 [a]

instance Queue Queue0 where
  empty = Q0 []
  head (Q0 (x:_)) = x  -- because haskell is lazy the snoc operation is a thunk and the concatenation happens only when call head so the complexity of head is O(n)
  tail (Q0 (_:xs)) = Q0 xs
  snoc (Q0 xs) x = Q0 (xs ++ [x])
