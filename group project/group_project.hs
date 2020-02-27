-- Team members
-- Junhyeok Jeong, jeongju@oregonstate.edu
-- Youngjoo Lee, leey3@oregonstate.edu
-- Ethan Mendelson, mendelse@oregonstate.edu

module Group_project where
import Prelude hiding (Num)

{- Feature menu
When designing your language, you must include some version of all of the following features.
1. Basic data types and operations. 
    You should provide at least boolean values and integers; you may want to include floating point numbers as well.
    You will need a way to represent both literal values and operations on these types. -}

--
-- *Abstract Syntax of Four

type Prog = [Four_Cmd]
type Function_name = String

data Value = PushN Int
         | PushB Bool
         | PushS String
         | ValueError
          deriving (Eq, Show)

type Var = (Varname Value)
data Flow_Cmd = PushN Int
         | PushB Bool
         | PushS String
         | PushV Var
         | Add
         | Minus
         | Mul
         | Equ
         | Larger
         | Smaller
         | IfElse Prog Prog
         | Loop Prog Prog
  deriving (Eq,Show)


-- Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 equ
--
ex1 :: Prog
ex1 = [PushN 3, PushN 4, Add, PushN 5, Equ]



-- Write a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 equ if 5 6 add else false end
--
ex2 :: Prog
ex2 = [PushN 3, PushN 4, Equ, IfElse [PushN 5, PushN 6, Add] [PushB False]]


-- Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
genAdd2 :: Int -> Int -> Prog
genAdd2 x y = [PushN x, PushN y, Add, Add]
           -- [PushN x, Add, PushN y, Add]
           -- [PushN (x+y), Add]  -- doing as much as possible at the metalanguage level


-- Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum :: [Int] -> Prog
genSum []     = [PushN 0]
genSum (x:xs) = genSum xs ++ [PushN x, Add]
  -- [PushN x] ++ genSum xs ++ [Add]  -- this one works but is memory inefficient
  -- genSum xs = [PushN (sum xs)] -- doing as much as possible at the metalanguage level



--
-- * Semantics of StackLang (now!)
--


-- Identify/define a semantics domain for Cmd and for Prog.
--
--    Things we need:
--      * stack
--        * int
--        * bool
--        * String
--      * error
data EitherFour a b c d = Left   a 
                        | Middle b     
                        | Right  c
                        | Four d

type Stack = [EitherFour Int String Bool Var]

type Domain = Stack -> Maybe Stack


-- Define the semantics of a StackLang command (ignore If at first).
cmd :: Four_Cmd -> Domain
cmd (PushN i)    = \s -> Just (Left i : s)
cmd (PushB b)    = \s -> Just (Right b : s)
cmd (PushS str)  = \s -> Just (Middle str : s)
cmd (PushV (n val))    = \s -> Just ((Four n (cmd val)) : s)
cmd Add         = \s -> case s of
                           (Left i : Left j : s') -> Just (Left (i+j) : s')
                           (Middle x : Middel y : s') -> Just (Middle (x++y) : s')
                           (Four (n (Left i)) : Left j : s') -> Just (Four (n (Left (i+j))) : s')
                           (Four (n (Left i)) : Four (u (Left j)) : s') -> Just (Four (n (Left (i+j))) : Four (u (Left j)) : s')
                           (Four (n (Middle i)) : Middle j : s') -> Just (Four (n (Middle (i++j))) : s')
                           (Four (n (Middle i)) : Four (u (Middle j)) : s') -> Just (Four (n (Middle (i++j))) : Four (u (Middle j)) : s')
                           _ -> Nothing
cmd Mul          = \s -> case s of
                           (Left i : Left j : s') -> Just (Left (i*j) : s')
                           (Four (n (Left i)) : Left j : s') -> Just (Four (n (Left (i*j))) : s' )
                           (Four (n (Left i)) : Four (u (Left j)) : s') -> Just (Four (n (Left (i*j))) : Four (u (Left j)) : s')
                           _ -> Nothing
cmd Equ         = \s -> case s of
                           (Left i  : Left j  : s') -> Just (Right (i == j) : s')
                           (Middle x : Middle y : s') -> Just (Right (x == y) : s')
                           (Right a : Right b : s') -> Just (Right (a == b) : s')              
                           (Four (n (Left i)) : Left j : s') -> Just (Four (n (Left i)) : Right(i == j) : s')
                           (Four (n (Left i)) : Four (u (Left j)) : s') -> Just (Four (n (Left i)) : Four (u (Left j)) : Right(i == j) : s')
                           (Four (n (Middle i)) : Middle j : s') -> Just (Four (n (Middle i)) : Right(i == j) : s')
                           (Four (n (Middle i)) : Four (u (Middle j)) : s') -> Just (Four (n (Middle i)) : Four (u (Middle j)) : Right(i == j) : s')
                           (Four (n (Right i)) : Right j : s') -> Just (Four (n (Right i)) : Right(i == j) : s')
                           (Four (n (Right i)) : Four (u (Right j)) : s') -> Just (Four (n (Right i)) : Four (u (Right j)) : Right(i == j) : s')
                           _ -> Nothing
cmd Larger      = \s -> case s of
                           (Left i  : Left j  : s') -> Just (Right (i > j) : s')
                           (Four (n (Left i)) : Left j : s') -> Just (Four (n (Left i)) : Right(i > j) : s')
                           (Four (n (Left i)) : Four (u (Left j)) : s') -> Just (Four (n (Left i)) : Four (u (Left j)) : Right(i > j) : s')
                           _ -> Nothing
cmd Smaller     = \s -> case s of
                           (Left i  : Left j  : s') -> Just (Right (i < j) : s')
                           (Four (n (Left i)) : Left j : s') -> Just (Four (n (Left i)) : Right(i < j) : s')
                           (Four (n (Left i)) : Four (u (Left j)) : s') -> Just (Four (n (Left i)) : Four (u (Left j)) : Right(i < j) : s')                           
                           _ -> Nothing
cmd (IfElse t e) = \s -> case s of
                           (Right True  : s') -> prog t s'
                           (Right False : s') -> prog e s'
                           _ -> Nothing
cmd (Loop c r) = \s -> case s of -- it's while loop (not do-while)
                           (Left i : s') -> Nothing -- Infinity loop
                           (Middle i : s') -> Nothing -- Infinity loop
                           (Right True : s') -> Nothing -- Infinity loop
                           (Four (n (Right True)) : Loop : s') -> Nothing -- Infinity loop
                           (Right False : s') -> prog s'
                           (Four (n (Right False)) : Loop : s') -> prog s'
                           (Four (n (Left i)) : Loop : s') -> prog (loop (Four (n (Left i))) c r) s'
--                           (Four (n (Left i)) : Loop : Left j : Larger : s') -> loop c r s'
--                           (Four (n (Left i)) : Loop : Left j : Equ : s') -> loop c r s'
--                           (Four (n (Middle i)) : Loop : s') -> prog (loop (Four (n (Middle i))) c r) s'
--                           (Four (n (Middle i)) : Loop : Left j : Smaller : s') -> loop c r s'
                           _ -> Nothing

-- 8. Define the semantics of a StackLang program.
prog :: Prog -> Domain
prog []    = \s -> Just s
prog (c:p) = \s -> case cmd c s of
                     Just s' -> prog p s'
                     _ -> Nothing


-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right False]
--
--   >>> run (genSum [1..10])
--   Just [Left 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
run :: Prog -> Maybe Stack
run p = prog p []


-- 2. Conditionals.
--    You should provide some way to branch in your language (e.g. if-then-else).
conditions :: Prog -> Domain
conditions = undefined
-- Conditions with comparing two strings

-- Conditions with int

-- conditions with T/F



-- 3. Recursion/loops. 
--    You should provide some way to loop in your language, either through an explicit looping construct (e.g. while) or through recursive functions.

-- ex) ["Loop", condition, result]
-- [Loop, PushN 3, PushN 4, Larger, ]
loop :: Four_Cmd -> Four_Cmd -> Four_Cmd -> Prog
loop (Four (n (Left i))) [PushN j, Larger] [PushN k, Add] = if (i > j) then [Four (n (Left i))] 
                                                            else [(Four (n (Left (i+k)))), Loop, [PushN j, Larger], [PushN k, Add]]
loop (Four (n (Left i))) [PushN j, Smaller] [PushN k, Minus] = if (i < j) then [Four (n (Left i))]
                                                            else [(Four (n (Left (i-k)))), Loop, [PushN j, Smaller], [PushN k, Minus]]
loop (Four (n (Left i))) [PushN j, Equ] [PushN k, Add] = if (i == j) then [Four (n (Left i))]
                                                         else [(Four (n (Left (i+k)))), Loop, [PushN j, Equ], [PushN k, Add]]
loop (Four (n (Left i))) [PushN j, Equ] [PushN k, Minus] = if (i == j) then [Four (n (Left i))]
                                                         else [(Four (n (Left (i-k)))), Loop, [PushN j, Equ], [PushN k, Minus]]
-- loop (Four (n (Middle i))) [PushS j, Equ] [PushS k, Add] = [(Four (n (Left (i+k)))), Loop, [PushN j, Larger], [PushN k, Add]]



-- 4. Procedures/functions with arguments (or some other abstraction mechanism).
--    You should provide a way to factor out repeated code and give it a name so that it can be reused. 
--    For imperative/functional languages, you must decide what kind of parameter passing scheme to use, which we’ll discuss in class. (Passing arguments is trivial for stack-based languages since arguments are passed on the stack!).


-- 5. Stack manipulation operations (stack-based languages only). 
--    You should provide a set of basic operations for manipulating values on the stack. You may want to look at a set of Forth stack maneuvers for inspiration. -}

{-Additionally, you must include at least 3 points worth of the following features. The point value of each feature is indicated in parentheses after the feature name.
1. Strings and operations (1).
    This feature would enable creating and manipulating string values, such as “hello world!”. The set of operations is up to you, but should must include at least concatenation.

2. Tuples and operations (1).
    This feature would enable creating tuples of other values, such as (2,true). 
    You should also be able to represent tuples containing tuples, such as (2,(true,"hello")).
    The set of operations is up to you but must include at least operations to get the first and second elements from the tuple.

3. List/array data type and operations (2). 
    This feature would enable creating and manipulating lists of values. Your operations should include standard operations such as indexing and (for lists) concatenation. 
    Your language must also be able to process lists in some way, for example, by looping over them or through recursive pattern matching.

4. User-defined data types and pattern matching (3).
    This feature would enable defining new recursive data types, such as lists or trees, at the library level. 
    Your language must all support processing these new data types in some way, such as through recursion and pattern matching.

5. First-class functions (2). 
    This feature would enable representing functions as values that can be passed as arguments to other functions or stored in tuples, lists, or other data types. 
    Your language must of course provide a way to both create and apply these functions.

6. Static type system (2).
    This feature would enable statically checking your program for errors before running it. 
    For a stack-based language, it would ensure that the stack never underflows and that operations are always applied with values of the correct type on the stack; 
    for imperative and functional languages the type system would ensure that there are no type errors caused by applying an operation, function, or procedure to an argument of the wrong type.

7. Input/output (2).
    This feature would enable reading and/or printing output from programs in your language. 
    This feature is tricky to implement in pure Haskell, but you can simulate it by extending your semantic domain with strings (or lists of strings) that represent input/output to/from your program.

8. Some other feature of your choice (?). 
    You’re free to pick some other feature I haven’t listed here! 
    In your milestone submission, be clear about what feature you’re picking and how many points you think it should be worth. We’ll negotiate from there, if needed. 
    (Minor constraint: You must have at least one 2-point or higher feature. 
    In other words, you can’t just add another simple type of value to a language with strings and tuples and call it a day!) -}