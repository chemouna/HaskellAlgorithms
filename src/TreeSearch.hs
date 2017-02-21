module TreeSearch where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Monoid
import           Data.Sequence
import           Data.Traversable
import           Prelude             hiding (foldl)
import           Test.QuickCheck


data Node a = Leaf a
            | Branch (Node a) (Node a)
            deriving (Show, Eq)

instance Functor Node where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch left right) =
      Branch (fmap f left) (fmap f right)

instance Foldable Node where
  foldMap f (Leaf v) = f v
  foldMap f (Branch left right) =
    foldMap f left `mappend` foldMap f right

instance (Arbitrary a) =>  Arbitrary (Node a) where
    arbitrary = sized tree where
        tree 0 = liftM Leaf arbitrary
        tree n | n > 0 =
                    oneof [liftM Leaf arbitrary,
                           liftM2 Branch subtree subtree]
                | otherwise = undefined  where
                    subtree = tree (n `div` 2)


-- Expand represents the operation of exploring the
-- current active set of nodes.
type Expand a =
    Seq (Node a) -> Node a -> Node a -> Seq (Node a)

search :: Eq a => Expand a -> a -> Seq (Node a) -> Bool
search expand target queue =
    let recur = search expand target
    in case viewl queue of
         EmptyL -> False
         Leaf value :< xs -> (value == target) || recur xs
         Branch left right :< xs ->
             recur (expand xs left right)

bfs :: Eq a => a -> Node a -> Bool
bfs target root =
    search (\xs l r -> xs |> l |> r) target (singleton root)

dfs :: Eq a => a -> Node a -> Bool
dfs target root =
    search (\xs l r -> l <| r <| xs) target (singleton root)

folding :: Eq a => a -> Node a -> Bool
folding target =
    foldl (\found current -> found || current == target) False

correctness :: Eq a => a -> Node a -> Bool
correctness t a = folding t a == bfs t a && bfs t a == dfs t a

main :: IO ()
main = verboseCheck (correctness :: (Int -> Node Int -> Bool))
