
-- codejam.hs
module CodeJam (codeJam, codeJamLiner, mrw) where

-- For Code Jam problems with unusual inputs.
-- Splits the input into lines, but the rest is up to the user.
codeJamWith :: ([String] -> [String]) -> IO ()
codeJamWith f = interact $ unlines . zipWith (++)
  ["Case #" ++ show t ++ ": " | t <- [1..]] . f . lines

-- For Code Jam problems where the first line contains the number of cases.
-- The given function should take an input s and return a tuple (r, t) where
-- r is the remaining unparsed input and t is the answer for the first case
-- in the string s.
codeJam :: ([String] -> ([String], String)) -> IO ()
codeJam f = codeJamWith $ let
  jamf _ [] = []
  jamf f s = let (r, t) = f s in t : jamf f r
  in jamf f . tail

-- For Code Jam problems where each case fits on a single line.
codeJamLiner :: (String -> String) -> IO ()
codeJamLiner f = codeJam (\(x:xs) -> (xs, f x))

-- We seem to need `map read . words` often.
mrw :: Read a => String -> [a]
mrw = map read . words



