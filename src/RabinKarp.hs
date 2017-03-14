module RabinKarp where

import Data.List
import Data.Char
import Data.Maybe

asciis :: String -> [Integer]
asciis = map (fromIntegral . ord)

hash :: Num a => [a] -> a
hash = sum . zipWith (*) (iterate (* 256) 1) . reverse

-- we need the hashes of all the pattern-length substrings of the search string.
hashes :: Int -> String -> [Integer]
hashes p xs = scanl (\s (f,t) -> 256*s - 256^p*f + t) (hash $ take p ascs) .
              zip ascs $ drop p ascs where ascs = asciis xs

rabinKarp :: String -> Maybe Int -> String -> Maybe Int
rabinKarp p s = lookup (hash $ asciis p) . drop (fromMaybe 0 s) .
                flip zip [0..] . hashesOfText
                where hashesOfText = hashes (length p)

test :: (String -> Maybe Int -> String -> Maybe Int) -> IO ()
test f = do assertEqual (f ""   Nothing  "Hello World") (Just 0)
            assertEqual (f "He" Nothing  "Hello World") (Just 0)
            assertEqual (f "od" Nothing  "Bonsai Code") (Just 8)
            assertEqual (f "ef" Nothing  "Bonsai Code") Nothing
            assertEqual (f ""   (Just 1) "Hello World") (Just 1)
            assertEqual (f "He" (Just 1) "Hello World") Nothing
            assertEqual (f "od" (Just 1) "Bonsai Code") (Just 8)
            assertEqual (f "ef" (Just 1) "Bonsai Code") Nothing
         where assertEqual a b = print (a == b, a, b)

main :: IO ()
main = test rabinKarp
