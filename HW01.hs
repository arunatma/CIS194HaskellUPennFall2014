{-# OPTIONS_GHC -Wall #-}
import Data.List

-- Solutions for http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf
-- https://www.dropbox.com/s/rkccfj41hwpqhps/01-intro.pdf?dl=0

-- Ran hlint HW01.hs and suggestions applied

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10 

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = intercalate [] [toDigits (dropLastDigit x), [lastDigit x]]

-- takes in two lists (of any type) and interleaves them and gives a single list
interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys
    
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ 
    zipWith (*) (reverse xs) 
                (interleave (repeat 1) (repeat 2))

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ intercalate [] (map toDigits xs)

validate :: Integer -> Bool
validate x 
    | reminder == 0 = True
    | otherwise = False
    where reminder = sumDigits (doubleEveryOther $ toDigits x) `mod` 10
    

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = 
    intercalate [] [hanoi (n - 1) a c b, hanoi 1 a b c, hanoi (n - 1) c b a]
    