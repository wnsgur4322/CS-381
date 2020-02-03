module TypesPractice where

data Result = OK Int | Error
  deriving (Eq,Show)

isOK :: Result -> Bool
isOK (OK _) = True
isOK Error  = False

add :: Int -> Result -> Result
add i (OK j) = OK (i+j)
add _ Error  = Error

-- From the Prelude:
--   map :: (a -> b) -> [a] -> [b]
--   (.) :: (b -> c) -> (a -> b) -> a -> c

-- Write the type of the following expressions,
-- or write "type error" if it is not type correct:

ex1 = OK -- :: Int -> Result
ex2 = Error -- :: Result
ex3 = isOK Error -- :: Bool
-- * ex4 = isOK 3 -- :: TYPE ERROR
-- isOK :: Result -> Bool
-- 3 :: Int
-- if doesn't make isOK (OK 3), then it works like this (isOK OK) 3 -> TYPE ERROR
-- * ex5 = isOK OK 3 -- :: TYPE ERROR
-- isOK :: Result -> Bool
-- OK :: Int

ex6 = isOK . OK
-- isOK :: Result -> Bool
-- (.) :: (b -> c) -> (a -> b) -> (a -> c) => b = Result, c = Bool, a = Int, d = Result
-- 
ex7 = add 3 -- :: Result -> Result
-- add :: Int -> Result -> Result
-- 3 :: Int
-- => Result -> Result because 3 matched to Int in 'add'

ex8 = add 3 Error
-- add :: Int -> Result -> Result
-- 3 :: Int
-- add 3 :: Result -> Result
-- Error :: Result

-- * ex9 = (add 3) OK
-- add :: Int -> Result -> Result
-- 3 :: Int
-- add 3 :: Result -> Result
-- OK :: Int -> Result
-- TYPE ERROR

ex10 = add 3 (OK 4)
ex11 = map OK
-- map :: (a -> b) -> [a] -> [b]
-- OK :: Int -> Result
-- a = Int, b = Result
-- [a] -> [b] => [Int] -> [Result]

ex12 = map isOK
-- map :: (a -> b) -> [a] -> [b]
-- isOK :: Result -> Bool
-- a = Result, b = Bool
-- [a] -> [b] => [Result] -> [Bool]

ex13 = map (add 3)
-- map :: (a -> b) -> [a] -> [b]
-- (add 3) :: Result -> Result
-- a = Result, b = Result
-- [a] -> [b] => [Result] -> [Result]

ex14 = map (add 3 . OK)
-- map :: (a -> b) -> [a] -> [b]
-- part 2 (add 3 . OK) can be (.) (add 3) OK
-- (.) :: (b -> c) -> (a-> b) -> a -> c
-- b = Result, c = Result from add 3
-- add 3 :: Result -> Result
-- (a -> Result) -> a -> Result
-- OK :: Int -> Result
-- a = Int
-- from map (a -> b)
-- from (.) (add 3) OK => Int -> Result
-- (Int -> Result) -> [Int] -> [Result]

ex15 = map (add 3) . map OK
-- part 1: map (add 3) :: [Result] -> [Result]
-- part 2: . :: (b -> c) -> (a-> b) -> a -> c
-- part 3: map OK :: [Int] -> [Result]
-- we can change from map (add 3) . map OK to (.) (map (add 3)) (map OK)
-- or start from back: [Result] <- [Result] . [Result] <- [Int]
-- => . remove middle repeated = remove [Result] . [Result]
-- => Answer: [Int] -> [Result]
-- b = [Result], c = [Result]
-- a = [Int], b = [Result]
-- [Int] -> [Result]