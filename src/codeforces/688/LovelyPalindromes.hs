
module LovelyPalindromes where

main :: IO ()
main = do
  s <- getLine
  putStrLn $ s ++ reverse s
