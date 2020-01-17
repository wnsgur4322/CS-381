module Basics where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :help, :load, :reload, :quit, :type, :info
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions


---------------------
-- Basic Functions --
---------------------

-- * function types
-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
double :: Int -> Int
double x = x + x

-- | Is this integer zero?
isZero :: Int -> Bool
-- option 1
-- isZero x = x == 0

-- option 2
isZero 0 = True
isZero x = False
-- or isZero _ = False

-- | Is this integer non-zero?
isNonZero :: Int -> Bool
-- option 1
-- isNonZero 0 = False
-- isNonZero _ = True

-- option 2
-- isNonZero x = not (isZero x)

-- option 3
-- isNonZero x = x /= 0

-- option 4: function composition
isNonZero = not . isZero


-- | Computes the average of two floating point numbers.
avg :: Float -> Float -> Float
avg x y = (x + y) / 2.0

-- | Uses avg to compute half of a floating point number.
half :: Float -> Float
-- option 1
-- half x = avg x 0.0
-- option 2: partial application
half = avg 0.0





-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
-- * anonymous functions


----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name
--   * a set of cases, each with:
--     * a data constructor
--     * zero or more arguments
-- * more pattern matching
--   * top-level and case-expressions

-- | An example data type with two cases.
data Result = OK Int | Error
  deriving (Eq,Show)

-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv _ 0 = Error
safeDiv x y = OK (x `div` y)

-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
-- way 1
-- fromResult (OK x) = x
-- fromResult Error = 0
-- param matching 2
fromResult r = case r of
  Error -> 0
  OK x -> x

-- | Add two results.
addResults :: Result -> Result -> Result
addResults (OK x) (OK y) = OK (x + y)
addResults _ _ = Error


-- The definition of Bool in the Haskell Prelude looks like this:
--   
--   data Bool = False | True


-- The type Result is similar to the Maybe type in the Prelude:
--
-- data Maybe a = Just a | Nothing



---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List
   = Nil --empty list
   | Cons Int List
  deriving (Eq,Show)

-- | The empty list.
empty :: List
empty = Nil

-- | The list: [2,3,4]
exList :: List
exList = Cons 2 (Cons 3 (Cons 4 Nil))

-- | Compute the length of a list.
listLength :: List -> Int
listLength Nil = 0
listLength (Cons h t) = 1 + listLength t 

-- | Compute the sum of the integers in a list.
listSum :: List -> Int
listSum Nil = 0
listSum (Cons h t) = h + listSum t


-- Example evaluation:
--
--  listSum (Cons 3 (Cons 4 Nil))
--  => 3 + listSum (Cons 4 Nil)
--  => 3 + (4 + listSum Nil)
--  => 3 + (4 + 0)
--  =>* 7




-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [a]
--    = []         -- Nil
--    | a : [a]    -- Cons

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]

-- List
-- Cons == ‘:’
-- 2 : 3 : 4 : [] => [2,3,4]
-- [2,3,4] ++ [5,6] => [2,3,4,5,6]



-- | Compute the length of a list.
length :: [a] -> Int
length [] = 0
length (h:t) = 1 + length t

-- | Compute the sum of an integer list.
-- sum :: [Int] -> Int
sum :: Num a => [a] -> a
sum [] = 0
sum (h:t) = h + sum t

-- | Compute the product of the elements in a list.
-- product :: [Int] -> Int
product :: Num a => [a] -> a
product [] = 1
product (h:t) = h * product t

allOdd :: [Int] -> Bool
allOdd [] = True
allOdd (h:t) = odd h && allOdd t

-- | Double all the elements in an integer list.
doubleAll :: Num a => [a] -> [a]
doubleAll [] = []
doubleAll (h:t) = 2 * h : doubleAll t

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll [] = []
notAll (h:t) = not h : notAll t

-- | Apply the even function to all elements in the list.
evenAll :: [Int] -> [Bool]
evenAll [] = []
evenAll (h:t) = even h : evenAll t


----------------------------
-- Higher-Order Functions --
----------------------------

-- * map and foldr


-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (h:t) = f h : map f t 

-- | Reimplement doubleAll using map.
doubleAll' :: Num a => [a] -> [a]
doubleAll' = map (\x -> 2 * x)
-- doubleAll' = map (2*)
-- doubleAll' xs = map (\x -> 2 * x) xs

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not
-- notAll' = map (\b -> not b)

-- | Reimplement evenAll using map.
evenAll' :: [Int] -> [Bool]
evenAll' = map even
-- evenAll' = map(\x -> even x)

-- | Fold an accumulator function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (h:t) = f h (foldr f x t) 

-- | Reimplement sum using foldr.
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: Num a => [a] -> a
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = undefined

-- | Reimplement allOdd using foldr.
allOdd' :: [Int] -> Bool
allOdd' = foldr (\i b -> odd i && b) True

-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = undefined