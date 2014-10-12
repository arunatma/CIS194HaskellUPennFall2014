{-# OPTIONS_GHC -Wall #-}
{-
Name: Arunram A

This is the solutions file for:
http://www.seas.upenn.edu/~cis194/hw/04-poly.pdf
Also available at https://www.dropbox.com/s/fnresmy6en1m6sx/04-poly.pdf?dl=0
-}

module HW04 where

import BST
import Data.Char
import Data.List

-- Exercise 1    
ex1 :: a -> b -> b
ex1 _ b = b
-- only possible implementation

-- Exercise 2
ex2 :: a -> a -> a
ex2 _ a = a
-- only possible implementation

-- Exercise3
ex3 :: Int -> a -> a
ex3 _ a = a
-- only possible implementation

-- Exercise 4
ex4 :: Bool -> a -> a -> a
ex4 True x _ = x
ex4 False _ x = x
-- only two possible variations (switch True and False above to get the other)

-- Exercise 4
ex5 :: Bool -> Bool
ex5 a = a
-- Only two variations (the other is ex5 a = not a)

-- Exercise 6
-- ex6 :: (a -> a) -> a
-- ?? Impossible

-- Exercise 7
ex7 :: (a -> a) -> a -> a
ex7 f x = f x
-- no other implementation possible

-- Exercise 8
ex8 :: [a] -> [a]
ex8 = map id
-- no other implementation possible

-- Exercise 9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map
-- ex9 is just the type signature of map

-- Exercise 10
ex10 :: Maybe a -> a
ex10 (Just x) = x
-- this function is impossible, as there is no way to treat "Nothing"

-- Exercise 11
ex11 :: a -> Maybe a
ex11 x = Just x

-- Exercise 12
ex12 :: Maybe a -> Maybe a
ex12 (Just x) = Just x
ex12 Nothing = Nothing

-- Exercise 13 Write the insertion method for a binary search tree:
-- insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
-- insertBST f x bt = []

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

extract :: Maybe Char -> [Char]
extract (Just x) = [x]
extract Nothing = []

headUpper :: Maybe Char -> Bool
headUpper (Just x) = isUpper x
headUpper Nothing = False

-- Exercise 14 Check to see if a list of strings contains only capitalized
-- words:
-- Examples:
-- allCaps ["Hi","There"] == True
-- allCaps [] == True
-- allCaps ["", "Blah"] == False
-- allCaps ["Hi","there"] == False
allCaps :: [String] -> Bool
allCaps [] = True
allCaps xs = all (== True) $ map headUpper $ (map safeHead xs)

-- Exercise 15 Drop the trailing whitespace from a string:
-- Examples:
-- dropTrailingWhitespace "foo" == "foo"
-- dropTrailingWhitespace "" == ""
-- dropTrailingWhitespace "bar   " == "bar"
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace xs = reverse $ dropWhile isSpace (reverse xs)

-- For removing spaces anywhere in the string
trimAllSpaces :: String -> String
trimAllSpaces = filter (not.isSpace)

-- For removing spaces at the beginning
trimStartSpaces :: String -> String
trimStartSpaces = dropWhile isSpace

-- Exercise 16 Get the first letter (if it exists) of a list of strings:
-- Examples:
-- firstLetters ["foo", "bar"] == [’f’,’b’]
-- firstLetters ["alpha",""] == [’a’]
-- firstLetters [] == []
-- firstLetters ["",""] == []
firstLetters :: [String] -> [Char]
firstLetters [] = []
firstLetters xs = concat $ map extract (map safeHead xs)

addBrackets :: String -> String
addBrackets xs = "[" ++ xs ++ "]"

-- Exercise 17 Render a proper bracketed list given a list of strings:
-- Examples:
-- asList ["alpha","beta","gamma"] == "[alpha,beta,gamma]"
-- asList [] == "[]"
-- asList ["lonely"] == "[lonely]"
asList :: [String] -> String
asList xs = addBrackets $ intercalate "," xs
