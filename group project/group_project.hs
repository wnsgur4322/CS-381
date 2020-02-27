-- Team members
-- Junhyeok Jeong, jeongju@oregonstate.edu
-- Youngjoo Lee, leey3@oregonstate.edu
-- Ethan Mendelson, mendelse@oregonstate.edu

module Four where
import Prelude hiding (Num)

{- Feature menu
When designing your language, you must include some version of all of the following features.
1. Basic data types and operations. 
    You should provide at least boolean values and integers; you may want to include floating point numbers as well.
    You will need a way to represent both literal values and operations on these types. -}

--
-- *Abstract Syntax of Four

type Prog = [Four_Cmd]
type Varname = String

data Value = PushVN Int
         | PushVB Bool
         | PushVS String
         | ValueError
          deriving (Eq, Show)
type Var = (Varname, Value)

-- After cmd for Var

--data ValueAfter = LeftI   a 
--                | MiddleS b     
 --               | RightB  c
                

type VarAfter = (Varname, EitherFour Int String Bool Var)
data Four_Cmd = PushN Int
         | PushB Bool
         | PushS String
         | PushV Var
         | Add
         | Sub
         | Mul
         | Equ
         | Larger
         | Smaller
         | IfElse Prog Prog
         | Loop Prog Prog
         | Dup
         | Drop
         | Swap
         | Over
         | Rot
  deriving (Eq,Show)

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
data EitherFour a b c d = LeftI   a 
                        | MiddleS b     
                        | RightB  c
                        | Four d
                        deriving (Eq, Show)
type Stackone = EitherFour Int String Bool Var
type Stack = [EitherFour Int String Bool Var]

type Domain = Stack -> Maybe Stack


-- Define the semantics of a StackLang command (ignore If at first).
cmd :: Four_Cmd -> Domain
cmd (PushN i)    = \s -> Just (LeftI i : s)
--cmd (PushVN i)    = \s -> Just (LeftI i : s)
cmd (PushB b)    = \s -> Just (RightB b : s)
--cmd (PushVB b)    = \s -> Just (RightB b : s)
cmd (PushS str)  = \s -> Just (MiddleS str : s)
--cmd (PushVS str)  = \s -> Just (MiddleS str : s)
--cmd (PushV (n, val))    = \s -> Just ((Four n, (cmd val)) : s)
cmd (PushV (n, val))    = \s -> Just ((Four (n, val)) : s)
cmd Add         = \s -> case s of
                           (LeftI i : LeftI j : s') -> Just (LeftI (i+j) : s')
                           (MiddleS x : MiddleS y : s') -> Just (MiddleS (x++y) : s')
                           (Four (n, (PushVN i)) : LeftI j : s') -> Just (Four (n, (PushVN (i+j))) : s')
                           (Four (n, (PushVN i)) : Four (u, (PushVN j)) : s') -> Just (Four (n, (PushVN (i+j))) : Four (u, (PushVN j)) : s')
                           (Four (n, (PushVS i)) : MiddleS j : s') -> Just (Four (n, (PushVS (i++j))) : s')
                           (Four (n, (PushVS i)) : Four (u, (PushVS j)) : s') -> Just (Four (n, (PushVS (i++j))) : Four (u, (PushVS j)) : s')
                           _ -> Nothing
cmd Sub         = \s -> case s of
                           (LeftI i : LeftI j : s') -> Just (LeftI (i-j) : s')
                           (Four (n, (PushVN i)) : LeftI j : s') -> Just (Four (n, (PushVN (i-j))) : s')
                           (Four (n, (PushVN i)) : Four (u, (PushVN j)) : s') -> Just (Four (n, (PushVN (i-j))) : Four (u, (PushVN j)) : s')
                           _ -> Nothing
cmd Mul          = \s -> case s of
                           (LeftI i : LeftI j : s') -> Just (LeftI (i*j) : s')
                           (Four (n, (PushVN i)) : LeftI j : s') -> Just (Four (n, (PushVN (i*j))) : s' )
                           (Four (n, (PushVN i)) : Four (u, (PushVN j)) : s') -> Just (Four (n, (PushVN (i*j))) : Four (u, (PushVN j)) : s')
                           _ -> Nothing
cmd Equ         = \s -> case s of
                           (LeftI i  : LeftI j  : s') -> Just (RightB (i == j) : s')
                           (MiddleS x : MiddleS y : s') -> Just (RightB (x == y) : s')
                           (RightB a : RightB b : s') -> Just (RightB (a == b) : s')              
                           (Four (n, (PushVN i)) : LeftI j : s') -> Just (Four (n, (PushVN i)) : RightB (i == j) : s')
                           (Four (n, (PushVN i)) : Four (u, (PushVN j)) : s') -> Just (Four (n, (PushVN i)) : Four (u, (PushVN j)) : RightB (i == j) : s')
                           (Four (n, (PushVS i)) : MiddleS j : s') -> Just (Four (n, (PushVS i)) : RightB (i == j) : s')
                           (Four (n, (PushVS i)) : Four (u, (PushVS j)) : s') -> Just (Four (n, (PushVS i)) : Four (u, (PushVS j)) : RightB (i == j) : s')
                           (Four (n, (PushVB i)) : RightB j : s') -> Just (Four (n, (PushVB i)) : RightB (i == j) : s')
                           (Four (n, (PushVB i)) : Four (u, (PushVB j)) : s') -> Just (Four (n, (PushVB i)) : Four (u, (PushVB j)) : RightB (i == j) : s')
                           _ -> Nothing
cmd Larger      = \s -> case s of
                           (LeftI i  : LeftI j  : s') -> Just (RightB (i > j) : s')
                           (Four (n, (PushVN i)) : LeftI j : s') -> Just (Four (n, (PushVN i)) : RightB (i > j) : s')
                           (Four (n, (PushVN i)) : Four (u, (PushVN j)) : s') -> Just (Four (n, (PushVN i)) : Four (u, (PushVN j)) : RightB (i > j) : s')
                           _ -> Nothing
cmd Smaller     = \s -> case s of
                           (LeftI i  : LeftI j  : s') -> Just (RightB (i < j) : s')
                           (Four (n, (PushVN i)) : LeftI j : s') -> Just (Four (n, (PushVN i)) : RightB (i < j) : s')
                           (Four (n, (PushVN i)) : Four (u, (PushVN j)) : s') -> Just (Four (n, (PushVN i)) : Four (u, (PushVN j)) : RightB (i < j) : s')                           
                           _ -> Nothing
cmd (IfElse t e) = \s -> case s of
                           (RightB True  : s') -> prog t s'
                           (RightB False : s') -> prog e s'
                           _ -> Nothing
cmd (Loop c r) = \s -> case s of -- it's while loop (not do-while)
                           (LeftI i : s') -> Nothing -- Infinity loop
                           (MiddleS i : s') -> Nothing -- Infinity loop
                           (RightB True : s') -> Nothing -- Infinity loop
                           (Four (n, (PushVB True)) : s') -> Nothing -- Infinity loop
                           (RightB False : s') -> Just s'
                           (Four (n, (PushVB False)) : s') -> Just ((Four (n, (PushVB False))) : s')
                           (Four (n, (PushVN i)) : s') -> Just ((loop (Four (n, (PushVN i))) c r) : s')
--                           (Four (n (Left i)) : Loop : Left j : Larger : s') -> loop c r s'
--                           (Four (n (Left i)) : Loop : Left j : Equ : s') -> loop c r s'
--                           (Four (n (Middle i)) : Loop : s') -> prog (loop (Four (n (Middle i))) c r) s'
--                           (Four (n (Middle i)) : Loop : Left j : Smaller : s') -> loop c r s'
                           _ -> Nothing
cmd Dup         = \s -> case s of
                           (LeftI i : s') -> Just (LeftI i : LeftI i : s')
                           (MiddleS x : s') -> Just (MiddleS x : MiddleS x : s')
                           (RightB a : s') -> Just (RightB a : RightB a : s')              
                           (Four (n, (PushVN i)) : s') -> Just (Four (n, (PushVN i)) : Four (n, (PushVN i)) : s')
                           (Four (n, (PushVS i)) : s') -> Just (Four (n, (PushVS i)) : Four (n, (PushVS i)) : s')
                           (Four (n, (PushVB i)) : s') -> Just (Four (n, (PushVB i)) : Four (n, (PushVB i)) : s')
                           _ -> Nothing
cmd Drop         = \s -> case s of
                           (LeftI i : s') -> Just (s')
                           (MiddleS x : s') -> Just (s')
                           (RightB a : s') -> Just (s')              
                           (Four (n, (PushVN i)) : s') -> Just (s')
                           (Four (n, (PushVS i)) : s') -> Just (s')
                           (Four (n, (PushVB i)) : s') -> Just (s')
                           _ -> Nothing
cmd Swap         = \s -> case s of
--                           (LeftI i  : LeftI j  : s') -> Just (LeftI j  : LeftI i : s')
--                           (LeftI i  : MiddleS x  : s') -> Just (MiddleS x  : LeftI i : s')
--                           (LeftI i  : RightB a  : s') -> Just (RightB a  : LeftI i : s')
--                           (LeftI i  : Four a : s') -> Just (Four a : LeftI i : s')
                           (LeftI i  : t  : s') -> Just (t  : LeftI i : s')
                           (MiddleS x : t : s') -> Just (t : MiddleS x : s')
                           (RightB a : t : s') -> Just (t : RightB a : s')              
                           (Four a : t : s') -> Just (t : Four a : s')
                           _ -> Nothing
cmd Over     = \s -> case s of
                           (x : y : s') -> Just (x : y : x : s')
                           _ -> Nothing
cmd Rot     = \s -> case s of
                           (x : y : z : s') -> Just (y : z : x : s')
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
-- You should provide some way to branch in your language (e.g. if-then-else).
-- The condtion (which treats string, bool, and integer types) is defined in above cmd (IfElse)


-- 3. Recursion/loops. 
--    You should provide some way to loop in your language, either through an explicit looping construct (e.g. while) or through recursive functions.
-- Loops (While loop) for Integers.
loop :: Stackone -> Prog -> Prog -> Stackone
loop (Four (n, (PushVN i))) [PushN j, Larger] [PushN k, Add] = if (i > j) then Four (n, (PushVN i)) 
                                                            else loop (Four (n, (PushVN (i+k)))) [PushN j, Larger] [PushN k, Add]
loop (Four (n, (PushVN i))) [PushN j, Smaller] [PushN k, Sub] = if (i < j) then Four (n, (PushVN i))
                                                            else loop (Four (n, (PushVN (i-k)))) [PushN j, Smaller] [PushN k, Sub]
loop (Four (n, (PushVN i))) [PushN j, Equ] [PushN k, Add] = if (i == j) then Four (n, (PushVN i))
                                                         else loop (Four (n, (PushVN (i+k)))) [PushN j, Equ] [PushN k, Add]
loop (Four (n, (PushVN i))) [PushN j, Equ] [PushN k, Sub] = if (i == j) then Four (n, (PushVN i))
                                                         else loop (Four (n, (PushVN (i-k)))) [PushN j, Equ] [PushN k, Sub]

exloop_good1 :: Prog
exloop_good1 = [PushV ("LoopGood1", PushVN 4), Loop [PushN 5, Larger] [PushN 1, Add]]

exloop_good2 :: Prog
exloop_good2 = [PushV ("LoopGood2", PushVN 4), Loop [PushN 2, Smaller] [PushN 1, Sub]]

exloop_bad1 :: Prog
exloop_bad1 = [PushV ("LoopBad1", PushVB True), Loop [] [], PushN 5, PushN 1, Add]

exloop_bad2 :: Prog
exloop_bad2 = [PushV ("LoopBad2", PushVS "HELLO"), Loop [] [], PushN 5, PushN 1, Add]

-- 4. Procedures/functions with arguments (or some other abstraction mechanism).
--    You should provide a way to factor out repeated code and give it a name so that it can be reused. 
--    For imperative/functional languages, you must decide what kind of parameter passing scheme to use, which we’ll discuss in class. (Passing arguments is trivial for stack-based languages since arguments are passed on the stack!).


-- 5. Stack manipulation operations (stack-based languages only). 
--    You should provide a set of basic operations for manipulating values on the stack. You may want to look at a set of Forth stack maneuvers for inspiration. -}
ex_dupgood :: Prog
ex_dupgood = [PushN 4, Dup]

ex_dupbad :: Prog
ex_dupbad = [Dup, Add]

ex_dropgood :: Prog
ex_dropgood = [PushN 4, PushN 5, Drop]

ex_dropbad :: Prog
ex_dropbad = [PushN 4, PushN 5, Drop, Add]

ex_swapgood :: Prog
ex_swapgood = [PushN 4, PushS "First", Swap]

ex_swapbad :: Prog
ex_swapbad = [PushN 4, Swap]

ex_overgood :: Prog
ex_overgood = [PushN 4, PushS "First", Over]

ex_overbad :: Prog
ex_overbad = [PushN 4, Over]

ex_rotgood :: Prog
ex_rotgood = [PushN 1, PushN 2, PushN 3, Rot]

ex_rotbad :: Prog
ex_rotbad = [PushN 1, PushN 2, Rot]

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