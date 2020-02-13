module ExamOne where
-- Semantic practice
-- EX 1)
-- i ::= (any integer)
-- s ::= inc i
--   |  reset
-- p ::= s ; p
--    |  ε
--
-- s; s; s; s; ε --> semiconlon is concrete syntax (similar to ; in C++ & Java)
-- inc 5 ; 

-- 1. What is a good semantic domain for statements?
-- Domain: Int -> Int
-- addTwo :: Int -> Int
-- addTwo x = x + 2

-- 2. Implement the language in Haskell by 
-- (a) encoding the abstract syntax as a Haskell data type,
type State = Int
type Val = State -> State

type I = Int
data S = Inc I | Reset
-- data P = Prog S P | Epsilon  -- > data P = <Constructor> S ; P
-- data List = Cons Int List | Nil
-- above both are the same
type P = [S]

-- (b) implementing valuation functions for both statements and programs.
valS :: S -> Val -- S for statement
valS (Inc a) = \s -> s + a
valS Reset = \s -> 0

ex1 = [Inc 5, Reset, Inc 2]
-- [Inc 5, Reset, Inc 2]
-- (Inc 5): Reset : (Inc 2) : []
--
-- s = Inc 5
-- p = Reset : (Inc 2) : []
-- s = Reset
-- p --> 0
-- s = Inc 2
-- p --> 2

valP :: [S] -> Int -- P for program P = [S]
valP [] = 0        -- since the prolbem description said "initial counter of 0", empty input list will return 0
valP (s:p) = valS s (valP p) -- Int -> Int

-- option 2
valP' :: [S] -> Int
valP' p = valP' (reverse p)

valP2 :: [S] -> Int -> Int
valP2 [] x = x
valP2 (s:p) x = valP2 p ((valS s) x)

-- option 2
-- valP2 (s:p) x =
--         let
--                 f = valS s
--                 newX = f x
--         in
--                 valP p newX

-- Ex 2)
data Cmd = Gas | Brake | Turn
type Prog = [Cmd]

type Pos = Int
type Speed = Int
data Dir = Forward | Backward

type StateRobot = (Pos, Maybe (Speed, Dir))
type ValRobot = StateRobot -> StateRobot


-- 1. What is a good semantic domain for commands?

-- 2. Implement semantic functions for Cmd and Prog.
flipD :: Dir -> Dir -- for Turn Cmd
flipD Forward = Backward
flipD Backward = Forward

semCmd :: Cmd -> ValRobot
semCmd Gas = \s ->
        case s of
                (p, Nothing) -> (p, Nothing)
                (p, Just (speed, Forward)) -> (p + speed, Just (speed+1, Forward))
                (p, Just (speed, Backward)) -> (p - speed, Just (speed+1, Backward))

semCmd Brake = \s ->
        case s of
                (p, Nothing) -> (p, Nothing)
                (p, Just (0, d)) -> (p, Just (0, d))
                (p, Just (speed, Forward)) -> (p + speed, Just (speed-1, Forward))
                (p, Just (speed, Backward)) -> (p - speed, Just ( speed-1, Backward))

semCmd Turn = \s ->
        case s of
                (p, Nothing) -> (p, Nothing)
                (p, Just (0, d)) -> (p, Just (0, flipD d))
                (p, Just _) -> (p, Nothing) -- *

semProg :: Prog -> StateRobot -> StateRobot
semProg [] x = x
semProg (c:p) x = semProg p (semCmd c x)


-- Syntax practice problem
-- A ::= 0 A 0
--    |  1 B 1
data A = OAO A
       | IBI B

-- B ::= B B C
--    |  0
data B = BBC B B C
       | O
-- C ::= C C C
--    |  1
data C = CCC C C C
       | I

ex2= BBC (BBC O O (CCC I I I)) O I -- 0011101
