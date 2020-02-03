module QUIZ1_PRACTICE where
import Prelude hiding (Maybe(..), isJust)

-- Quiz 1 extra class

-- Define an addition function for the peano naturals
data MyNat
        = O
        | S MyNat deriving Show -- deriving Show is for printing on terminal

myZero :: MyNat
myZero = O

-- O => 0
-- S n => n + 1
-- 0 + 1 + 1

myTwo :: MyNat
myTwo = S (S O)

-- isZero 0 = True
-- isZero myTwo = False
isZero :: MyNat -> Bool
isZero O = True
isZero n = False

-- S n = n + 1
-- myAdd m n + 1
-- S (myAdd m n)

myAdd :: MyNat -> MyNat -> MyNat -- take two MyNat and then output MyNat
myAdd O n = n
myAdd (S m) n = S (myAdd m n)

equal :: MyNat -> MyNat -> Bool
equal O O = True
equal O (S n) = False
equal (S m) O = False
equal (S m) (S n) = equal m n

inList :: Int -> [Int] -> Bool
inList n [] = False
inList n (x:xs) = if n == x then True else inList n xs
             -- = n == x || inList n xs

data Maybe a = Just a | Nothing deriving Show

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

safeDivide :: Int -> Int -> Maybe Int
safeDivide n 0 = Nothing
safeDivide n m = Just (n `div` m)

--divideThenAdd :: Int -> Int -> Int
--divideThenAdd n m =
--        case safeDivide n m of
--          Nothing -> 0
--          Just x -> x + 1 
