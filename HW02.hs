{-# OPTIONS_GHC -Wall #-}
{-
Name: Arunram A
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
This is the solutions file for:
http://www.seas.upenn.edu/~cis194/hw/02-dict.pdf
Also available at https://www.dropbox.com/s/4ytftmey6nmcd18/02-dict.pdf?dl=0        
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (s:ss) (hs) = s `elem` hs && formableBy ss (delete s hs)
-- Test Cases
-- formableBy "fun" ['x','n','i','f','u','e','l'] == True
-- formableBy "haskell" ['k','l','e','h','a','l','s'] == True
-- formableBy "haskell" ['k','l','e','h','a','y','s'] == False

wordsFrom :: Hand -> [String]
wordsFrom hs =  [w | w <- allWords, formableBy w hs]
-- Test Cases
-- wordsFrom ['a','b','c','d'] == ["ab","ad","ba","bad","cab","cad","dab"]
-- wordsFrom ['h','e','l','l','o'] == [ "eh","el","ell","he","hell","hello",
-- "helo", "ho","hoe","hole","lo","oe","oh","ole"]

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t h s 
    | (length t /= length s || templateMismatch t s)   = False
    | otherwise                             = formableBy (extractLetters t s) h
-- Test Cases
-- wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True
-- wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" == False
-- wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == False
-- wordFitsTemplate "let" ['x','x'] "let" == True
-- wordFitsTemplate "??r?" "cxeabcl" "able" == False

-- True if the string mismatches with the template
templateMismatch :: Template -> String -> Bool
templateMismatch t s = (length t - count '?' t) /= numMatchedElements 0 t s

-- count function to count a specific element in a given list
count :: Eq a => a -> [a] -> Int
count c = length . filter (\x -> x == c)

-- numMatchedElements compares template and string, gives total matched char.
numMatchedElements :: Int -> Template -> String -> Int
numMatchedElements acc _ [] = acc
numMatchedElements acc [] _ = acc
numMatchedElements acc (t:ts) (s:ss) 
    | t == s = numMatchedElements (acc + 1) ts ss
    | otherwise = numMatchedElements acc ts ss
    
-- extractLetters remove common letters between the template and given string
-- called by wordFitsTemplate (Test cases covered in that)
extractLetters :: Template -> String -> String
extractLetters [] ss = ss
extractLetters _ [] = []
extractLetters (t:ts) ss = extractLetters ts (delete t ss)

-- finds all words that match a given template, with a hand of tiles
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h =  [w | w <- allWords, wordFitsTemplate t h w]
-- Test Case
-- wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] ==
-- ["acre","bare","carb","care","carl","earl"]

scrabbleValueWord :: String -> Int
scrabbleValueWord s = sum (map scrabbleValue s)
-- Test Cases
-- scrabbleValueWord "care" == 6
-- scrabbleValueWord "quiz" == 22

-- given a list of words, select best words based on the points
bestWords :: [String] -> [String]
bestWords [] = []
bestWords ss = map (ss !!) (maxScores $ map scrabbleValueWord ss)
-- Test Cases
-- bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) 
--  == ["carb"]
-- bestWords ["cat", "rat", "bat"] == ["bat","cat"]
-- bestWords [] == []

maxScores :: Ord a => [a] -> [Int]
maxScores [] = []
maxScores ss = findIndices (== maximum ss) ss

-- given a S Template and a word, calculate the score
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate st w = (wordMultiplier 1 st) * (sum (zipWith (*) 
                             (map scrabbleValue w) (valueMultiplier st)))
-- Test Cases
-- scrabbleValueTemplate "?e??3" "peace" == 27
-- scrabbleValueTemplate "De?2?" "peace" == 24
-- scrabbleValueTemplate "??Tce" "peace" == 11

valueMultiplier :: STemplate -> [Int]
valueMultiplier [] = []
valueMultiplier (x:xs)
    | x == 'D' = 2 : valueMultiplier xs
    | x == 'T' = 3 : valueMultiplier xs
    | otherwise = 1 : valueMultiplier xs

wordMultiplier :: Int -> STemplate -> Int
wordMultiplier prod [] = prod
wordMultiplier prod (x:xs)
    | x == '2' = 2 * wordMultiplier prod xs
    | x == '3' = 3 * wordMultiplier prod xs
    | otherwise = 1 * wordMultiplier prod xs
    


