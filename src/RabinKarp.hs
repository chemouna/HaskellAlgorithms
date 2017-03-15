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
hashes p xs = scanl (\s (f,t) -> 256 * s - 256^p * f + t) (hash $ take p ascs) .
              zip ascs $ drop p ascs where ascs = asciis xs

rabinKarp :: String -> Maybe Int -> String -> Maybe Int
rabinKarp p s = lookup (hash $ asciis p) . drop (fromMaybe 0 s) .
                flip zip [0..] . hashesOfText
                where hashesOfText = hashes (length p)

-- Solution 2

rabinKarp2 :: String -> String -> Int
rabinKarp2 p text =
        if p == ""
                then -1
        else
                fromMaybe (-1) $ mapOnMaybe fst findHashThatMatchesPattern
        where m = length p
              firstTextWindowHash = hash2 text m
              textWindowPieces = windowed (m + 1) text -- windows of the text with the pattern size
              nextHash currentHash chars = reHash currentHash (head chars) (last chars) m
              textHashes = scanl nextHash firstTextWindowHash textWindowPieces
              textHashesWithIndexes = zip [0..] textHashes
              findHashThatMatchesPattern = find matchingString textHashesWithIndexes
              matchingString (offset, textHash) = hash2 p m == textHash && p == subString text offset m

-- the above can be written as in this way : i prefer the above and think its more readable
-- rabinKarp2 :: String -> String -> Int
-- rabinKarp2 text pattern =
--         if pattern == "" then -1
--         else fromMaybe (-1) $ mapOnMaybe fst $ find matchingString $ zip [0..] $ scanl nextHash (hash2 text m) $ windowed (m+1) text
--         where n = length text
--               m = length pattern
--               nextHash currentHash chars = reHash currentHash (head chars) (last chars) m
--               matchingString (offset, textHash) = hash2 pattern m == textHash && pattern == subString text offset m

mapOnMaybe :: (a -> b) -> Maybe a -> Maybe b
mapOnMaybe fn (Just x) = Just (fn x)
mapOnMaybe _ Nothing = Nothing

subString text start end = take end $ drop start text

windowed :: Int -> [a] -> [[a]]
windowed _ [] = []
windowed size ls@(_:xs) = if length ls >= size then take size ls : windowed size xs else windowed size xs

globalQ = 1920475943
globalR = 256

hash2 = hash' globalR globalQ
hash' r q string m = foldl (\acc x -> (r * acc + ord x) `mod` q) 0 $ take m string

reHash = reHash' globalR globalQ
reHash' r q existingHash firstChar nextChar m =
        (takeOffFirstChar `mod` fromIntegral q * fromIntegral r + ord nextChar) `mod` fromIntegral q
        where
                rm = if m > 0 then (fromIntegral r ^ fromIntegral (m-1)) `mod` fromIntegral q else 0
                takeOffFirstChar = existingHash - fromIntegral rm * ord firstChar


assertEqual a b = print (a == b, a, b)

test :: (String -> Maybe Int -> String -> Maybe Int) -> IO ()
test f = do assertEqual (f ""   Nothing  "Hello World") (Just 0)
            assertEqual (f "He" Nothing  "Hello World") (Just 0)
            assertEqual (f ""   (Just 1) "Hello World") (Just 1)
            assertEqual (f "He" (Just 1) "Hello World") Nothing

main :: IO ()
main = test rabinKarp
