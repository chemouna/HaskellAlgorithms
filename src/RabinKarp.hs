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

globalQ = 1920475943
globalR = 256

-- ((r^m-1 * ascii char) + (r^m-2 * ascii char) + … (r0 * ascii char)) % q
-- where r = 256, q = 1920475943
-- so for hash "markusaerelius" 3 it is:
-- ((256^2 * ord ‘m’) + (2561 * ord ‘a’) + (2560 * ord ‘r’)) `mod` 1920475943
hash' = hash'' globalR globalQ
hash'' r q string m = foldl (\acc x -> (r * acc + ord x) `mod` q) 0 $ take m string

reHash = reHash' globalR globalQ
reHash' r q existingHash firstChar nextChar m =
        (takeOffFirstChar `mod` fromIntegral q * fromIntegral r + ord nextChar) `mod` fromIntegral q
        where
                rm = if m >0 then (fromIntegral r ^ fromIntegral (m-1)) `mod` fromIntegral q else 0
                takeOffFirstChar = existingHash - fromIntegral rm * ord firstChar

windowed :: Int -> String -> [String]
windowed = undefined

-- subString :: String -> Int ->  Int -> Maybe Int 
-- subString = _

rabinKarp2 :: String -> String -> Int
rabinKarp2 text p =
        if p == "" then -1
        else fromMaybe (-1) $ mapToMaybe fst $ find matchingString $ zip [0..] $ scanl nextHash (hash' text m) $ windowed (m+1) text
        where n = length text
              m = length p
              patternHash = hash' p m
              nextHash currentHash chars = reHash currentHash (head chars) (last chars) m
              matchingString (offset, textHash) = patternHash == textHash && p == subString text offset m


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
