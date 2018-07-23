module Main where
import Data.Char


isPalindromez :: String -> Maybe String
isPalindromez "" = Nothing
isPalindromez xs =
	case xs == reverse xs of
		False -> Just "This is not a Palindrome."
		True -> Just "This is a Palindrome."

isWord :: String -> Maybe String
isWord "" = Nothing
isWord xs =
	case (lla isAlpha xs) of
		False -> Just "It is not Alpha"
		True -> Just "This is Alpha"
isWord xs = 
	case lla isAlpha xs of
		False -> Nothing
		True -> Just xs

checkInput :: String -> Maybe String
checkInput "" = Nothing
checkInput xs = 
	case (isWord . ignoreCapsAndSpaces) xs of
		Nothing -> Nothing
		Just xs' -> isPalindromez xs'

-- maybeMap :: (a -> b) -> Maybe a -> Maybe b
-- maybeMap function Nothing = Nothing
-- maybeMap function (Just x) = Just (function x)

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

ignoreSpaces :: String -> String
ignoreSpaces xs = concat (words xs)
ignoreSpaces1 xs = (concat . words) xs
ignoreSpaces2 xs = words xs
ignoreSpaces3 xs = (.) concat words  xs
ignoreSpaces4 xs = concat $ words $ xs

pam :: (a -> b) -> [a] -> [b]
pam function [] = []
pam function (x:xs) = function x : pam function xs

ignoreCaps :: String -> String
ignoreCaps string = pam toLower string
ignoreCapsAndSpaces string = (ignoreSpaces . ignoreCaps) string

lla :: (a -> Bool) -> [a] -> Bool
lla function [] = True
lla function (x:xs) =
	case (function x) of 
		False -> False
		True -> lla function xs

myAllFold :: (a -> Bool) -> [a] -> Bool
myAllFold function xs =
	foldr (\x y -> function x && y) True xs

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : map f xs

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just a) = Just (f a)

-- isWord :: String -> Bool
-- isWord xs = lla isAlpha xs

main :: IO ()
main = do
	word <- getLine
	print (isPalindrome (ignoreCapsAndSpaces word))
	print (isPalindrome (ignoreCaps (ignoreSpaces word)))


