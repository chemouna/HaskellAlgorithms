module BstLca where

data BST a = Node a (BST a) (BST a) | Nil
           deriving Show

bstContains :: Ord a => BST a -> a -> Bool
bstContains Nil _ = False
bstContains (Node root left right) value 
  | root == value = True
  | value < root = bstContains left value
  | otherwise = bstContains right value


-- Given a binary search tree and two values to search for, searches that BST
-- for the lowest common ancestor of the two values, then returns that node if
-- it exists.
bstLca :: Ord a => BST a -> a -> a -> Maybe (BST a)
bstLca Nil _ _ = Nothing
bstLca bst@(Node root left right) a b 
  | not (bstContains bst a) || not (bstContains bst b) = Nothing
  |  a == root || b == root = Just bst
  | root < a && root < b = bstLca left a b
  | otherwise =  bstLca right a b


-- a more correct version that handles all cases
bstLca2 :: Ord a => BST a -> a -> a -> Maybe (BST a)
bstLca2 Nil _ _ = Nothing
bstLca2 bst@(Node root left right) a b
  | a < root && b < root = bstLca2 left a b
  | a > root && b > root = bstLca2 right a b
  | a == root = if bstContains bst b then Just bst else Nothing
  | b == root = if bstContains bst a then Just bst else Nothing
  | bstContains bst a && bstContains bst b = Just bst
  | otherwise = Nothing
