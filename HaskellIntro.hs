{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
-- lastDigit = error "lastDigit not yet defined"
lastDigit x = rem x 10

dropLastDigit :: Integer -> Integer
-- dropLastDigit = error "dropLastDigit not yet defined"
dropLastDigit x = div x 10

toDigits :: Integer -> [Integer]
-- toDigits = error "toDigits not yet defined"
toDigits x = if x <= 0 then [] else (++) (toDigits (dropLastDigit x)) [(lastDigit x)] 

doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = error "doubleEveryOther not yet defined"
doubleEveryOther [] = []
doubleEveryOther xs = if (rem (length xs) 2) > 0 then (head xs) : (doubleEveryOther (tail xs)) else ((head xs) * 2) : (doubleEveryOther (tail xs))

sumDigits :: [Integer] -> Integer
-- sumDigits = error "sumDigits not yet defined"
sumDigits [] = 0
sumDigits xs = (if (head xs) > 9 then sumDigits (toDigits (head xs)) else head xs) + (sumDigits (tail xs))

validate :: Integer -> Bool
-- validate = error "validate not yet defined"
validate x = (rem (sumDigits (doubleEveryOther (toDigits x))) 10) == 0

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
-- pow = error "pow not yet defined"
pow f n = if n < 1 then id else f . (pow f (n - 1))

g :: Integer -> Integer
-- g = error "g not yet defined"
g 0 = 0
g n = n - ((pow g 2) (n - 1))

h :: Integer -> Integer
-- h = error "h not yet defined"
h 0 = 0
h n = n - ((pow h 3) (n - 1))

d :: Int -> Integer -> Integer
-- d = error "d not yet defined"
d i 0 = 0
d i n = n - ((pow (\m -> d i m) i) (n - 1))

--
-- Problem 3
--
powerSet :: Ord a => Set a -> Set (Set a)
-- powerSet = error "powerSet not yet defined"
powerSet s = if (Set.isEmpty s) then
                (Set.singleton Set.empty)
             else
                (Set.union (powerSet (snd (Set.split s))) (combineSet (Set.singleton (fst (Set.split s))) (powerSet (snd (Set.split s)))))

-- Combines singleton set with all other sets in set of sets
combineSet :: Ord a => Set a -> Set (Set a) -> Set (Set a)
combineSet s1 s2 = if (Set.isEmpty s2) then 
							(Set.singleton (Set.union s1 Set.empty)) 
						else 
							(Set.union (Set.singleton (Set.union s1 (fst (Set.split s2)))) (combineSet s1 (snd (Set.split s2))))





















