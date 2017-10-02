module NextNumber where

import Jam
import Data.List
import Control.Monad
import Data.Foldable
import Control.Applicative
import System.IO
import Text.Printf
import Data.Maybe

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

-- Solution 3
nice :: [Char] -> Bool
nice (a:as) = any (> a) as
nice [] = False

rearrange :: [Char] -> Maybe [Char]
rearrange (a:as)
        | Just ans <- rearrange as
                = Just (a:ans)
        | nice (a:as)
                = let b = minimum [b | b <- as, b > a] in
                        Just (b:sort (a: (as \\ [b])))
rearrange _ = Nothing

backup :: [Char] -> [Char]
backup xs
        | ys <- sort xs, y0 <- minimum [y | y <- ys, y /= '0']
                = y0:'0':(ys \\ [y0])

main' = do
        n <- liftM read getLine
        mapM_ (\ i -> do
                xs <- getLine
                putStrLn ("Case #" ++ show i ++ ": " ++ (fromMaybe (backup xs) (rearrange xs)))) [1..n]
