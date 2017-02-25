
module TreeTraversal where

data Tree a = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

preorder, inorder, postorder, levelorder :: Tree a -> [a]
preorder Empty = []
preorder (Node v l r) = v : preorder l ++ preorder r

inorder Empty = []
inorder (Node v l r) = inorder l ++ (v : inorder r)

postorder Empty = []
postorder (Node v l r) = postorder l ++ postorder r ++ [v]

levelorder Empty = []
levelorder (Empty:xs) = levelorder xs
levelorder (Node v l r:xs) = v:levelorder (xs ++ [l, r]) 
