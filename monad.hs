module Main where

import Data.Char

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

ignoreSpaces :: String -> String
ignoreSpaces xs = (concat . words) xs

ignoreCaps :: String -> String
ignoreCaps xs = map toLower xs

ignoreCapsAndSpaces :: String -> String
ignoreCapsAndSpaces xs = (ignoreCaps . ignoreSpaces) xs

main = do
  word <- getLine
  print (isPalindrome (ignoreCapsAndSpaces word))
