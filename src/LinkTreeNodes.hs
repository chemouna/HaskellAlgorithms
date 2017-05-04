
module LinkTreeNodes where

data Tree a =  Empty | Branch (Tree a) a (Tree a)
                deriving (Eq, Show)

left (Branch l _ _) = l
right (Branch _ _ r) = r
root (Branch _ n _) = n

link :: Eq a => Tree a -> [[a]]
link t = link2 [[t]] [[root t]] where
      link2 [] acc = reverse acc
      link2 (ts:tss) acc = let ts2 = concat [filter (/= Empty) [left tr, right tr]
                                            | tr <- ts] in
          if null ts2 then link2 tss acc
          else link2 (tss ++ [ts2]) (map root ts2:acc)


-- Branch (Branch Tree 2 2 Tree 2)
