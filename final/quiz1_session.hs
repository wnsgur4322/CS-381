import Prelude hiding (Maybe(..), isJust)

-- Problem 1: Define a data type MyNat, that represents numbers 0,1,2,...
--            Do so using the Peano encoding of numbers:
--
--            0 is a number (Hint: you can't make a constructor named 0. Try O?)
--            if n is a number, so is n + 1, or succ n.

-- Define an addition function for the peano naturals
data MyNat
    = O
    | S MyNat deriving Show

myZero :: MyNat
myZero = O

-- O => 0
-- S n => n + 1
--
-- 0 + 1 + 1 = 2

myTwo :: MyNat
myTwo = S (S O)

-- Problem 2: Given a MyNat, return True if it's 0, and False if it's any other number.
-- isZero O = True
-- isZero myTwo = False
isZero :: MyNat -> Bool
isZero O = True
isZero n = False


-- Problem 3: Write a function equal, which checks if two MyNat's are equal.
equal :: MyNat -> MyNat -> Bool
equal O O     = True
equal O (S n) = False
equal (S m) O = False
equal (S m) (S n) = equal m n


-- Problem 4: Write a function add, which adds two MyNats.
-- (m + 1) + n ==
-- 1 + (m + n)
--
-- S n = n + 1
-- (myAdd m n) + 1
-- S (myAdd m n)

myAdd :: MyNat -> MyNat -> MyNat
myAdd O n = n
myAdd (S m) n = S (myAdd m n)

-- Problem 5: Write a function inList, which checks if a number is in a list of numbers.
-- f(x,y) in Math
-- f x y in Haskell

inList :: Int -> [Int] -> Bool
inList _ [] = False
inList n (x:xs) = n == x || inList n xs

-- Problem 6: Define Maybe a! There's nothing special about Maybe from the standard library.
--            You can just as well declare it yourself. Try it!
--            If you don't want to, feel free to find a definition of
--            Maybe online to do the last few problems.

data Maybe a = Just a | Nothing

-- Problem 7: Define a function isJust, that tells you if a Maybe a is "Just" something,
--            or "Nothing". It should return False for Nothing values, and True
--            for Just values.

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True


-- Problem 8: Define a function "safeDivide", that divides one number by another number,
--            except when the other number is 0. When the number is 0, it should return
--            Nothing.
safeDivide :: Int -> Int -> Maybe Int
safeDivide n 0 = Nothing
safeDivide n m = Just (n `div` m)

-- Problem 9: Define function "divideThenAdd", **using safeDivide**, that
--            divides one number by another. If the division is successful,
--            this function should add 1 to the result of the division, and return
--            that. If the division fails (you get Nothing), it should return 0.
-- divideThenAdd x y = (x/y) + 1
divideThenAdd :: Int -> Int -> Int
divideThenAdd n m =
    case safeDivide n m of
        Nothing -> 0
        Just x -> x + 1