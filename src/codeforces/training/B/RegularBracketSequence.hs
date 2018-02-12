
module RegularBracketSequence where

import Data.List

f (a, b) '(' = (a, b + 1)
f (a, 0) ')' = (a, 0)
f (a, b) ')' = (a + 2, b - 1)

main = do getLine >>= print . fst . foldl f (0, 0)


