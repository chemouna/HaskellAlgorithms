module NextNumber where

import Jam
import Data.List
import Control.Monad
import Data.Foldable
import Control.Applicative
import System.IO
import Text.Printf

{--
main = jam $ do
  cs <- gets
  let spls = drop 2 $ reverse $ zip (inits cs) (tails cs) of
        pure $ case find (\(_, a:b:_) -> a < b) spls of
          Nothing         -> b:'0':as ++ bs
            where (as, b:bs) = span (== '0') $ reverse cs
          Just (xs, y:ys) -> xs ++ b:as ++ y:bs
            where (as, b:bs) = span (<= y) $ reverse ys
--}



main :: IO ()
main = do
  ntests <- read <$> getLine :: IO Int
  forM_ [1..ntests] $ \test -> do
   str <- getLine
   printf "Case #%d: %s\n" test (next str)

next :: String -> String
next s = result where Just result = next' s <|> next' ('0':s)

next' :: String -> Maybe String
next' (a:t@(b:_)) =     (a:) <$> next' t
                     <|> if a<b then Just (f a t) else Nothing
next' _ = Nothing

f :: Char -> String -> String
f a t = let nextLargest = head (sort (filter (>a) t))
            removed = sort (a:delete nextLargest t)
        in nextLargest:removed
