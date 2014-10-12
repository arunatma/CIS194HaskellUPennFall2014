{-# OPTIONS_GHC -Wall #-}
{-
Name: Arunram A

This is the solutions file for:
http://www.seas.upenn.edu/~cis194/hw/05-type-classes.pdf
Also available at https://www.dropbox.com/s/lxsdwtk47gkrfx8/05-type-classes.pdf?dl=0
-}

module HW05 where
    
import Ring
import Parser
import Data.Maybe    ( listToMaybe )

-- Exercise 1
intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
    (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
    (addId == (0 :: Integer))
    
-- Exercise 2
-- Modulo 5 operation forms a ring.
data Mod5 = MkMod Integer deriving (Show, Eq, Read)

instance Ring Mod5 where
  addId  = MkMod 0
  addInv (MkMod x) = MkMod ((negate x) `mod` 5)
  mulId  = MkMod 1

  add (MkMod x) (MkMod y) = MkMod (x + y `mod` 5)
  mul (MkMod x) (MkMod y) = MkMod (x * y `mod` 5)

instance Parsable Mod5 where
  parse = listToMaybe . reads 
  
mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "MkMod 3" == Just (MkMod 3 :: Mod5, "")) &&
    (parseRing "MkMod 1 + MkMod 2 * MkMod 5" == Just (MkMod 1 :: Mod5)) &&
    (addId == (MkMod 0 :: Mod5))
  
-- Exercise 3
-- Matrix arithmetic forms a ring.
data Matrix = MkMat Integer Integer Integer Integer deriving (Show, Eq, Read)

instance Ring Matrix where
  addId  = MkMat 0 0 0 0
  addInv (MkMat a b c d) = MkMat (negate a) (negate b) (negate c) (negate d)
  mulId  = MkMat 1 0 0 1

  add (MkMat a b c d) (MkMat e f g h) = 
    MkMat (a + e) (b + f) (c + g) (d + h)
  mul (MkMat a b c d) (MkMat e f g h) = 
    MkMat (a*e + b*g) (a*f + b*h) (c*e + d *g) (c*f + d*h)

instance Parsable Matrix where
  parse = listToMaybe . reads 
    
-- Test cases for Exercise 3
bool1 = addId == (MkMat 0 0 0 0 :: Matrix)      -- True
bool2 = 
  parseRing "MkMat 1 2 3 4 + MkMat 1 0 0 1 * MkMat 3 4 5 6" == 
    Just (MkMat 4 6 8 10 :: Matrix)
  
-- Exercise 4
-- Boolean arithmetic forms a ring.
data Boolean = MkBool Bool deriving (Show, Eq, Read)

xor :: Bool -> Bool -> Bool
xor x y 
    | x == y = False
    | otherwise = True
    
instance Ring Boolean where
  addId  = MkBool False
  addInv (MkBool x) = MkBool (not x) 
  mulId  = MkBool True

  add (MkBool x) (MkBool y) = MkBool (x `xor` y)
  mul (MkBool x) (MkBool y) = MkBool (x && y)
  
instance Parsable Boolean where
  parse = listToMaybe . reads 
    
-- Test cases for Exercise 4
bool3 = addId == (MkBool True)      -- Should be False
bool4 = 
  parseRing "MkBool True + MkBool True * MkBool False" == 
    Just (MkBool True :: Boolean)


-- Exercise 5
-- distribute multiplication over addition 
-- (both left distribute and right distribute)
-- distribute
distribute :: Ring a => RingExpr a -> RingExpr a
distribute = id
-- TODO


-- Exercise 6
-- Detect when multiplying by the multId and removes the multiplication
-- squashMulId
squashMulId :: Ring a => RingExpr a -> RingExpr a
squashMulId = id
-- TODO
    