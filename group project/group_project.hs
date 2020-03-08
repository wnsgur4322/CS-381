-- Team members
-- Junhyeok Jeong, jeongju@oregonstate.edu
-- Youngjoo Lee, leey3@oregonstate.edu
-- Ethan Mendelson, mendelse@oregonstate.edu

module StackProject where
import Prelude hiding (Num)

{- Feature menu
When designing your language, you must include some version of all of the following features.
1. Basic data types and operations. 
    You should provide at least boolean values and integers; you may want to include floating point numbers as well.
    You will need a way to represent both literal values and operations on these types. -}

--
-- *Abstract Syntax of Four

type Varname = String
type Var = (Varname, Val)

data Stack_Cmd = PushN Int
         | PushB Bool
         | PushS String
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
         | Let Varname  -- Below : For Repeated functions and values
         | Ref Varname
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
--        * Var
--      * error

-- | Values.
data Val
   = LeftI Int                 -- integer
   | RightB Bool
   | MiddleS String
   | V Var
   | FError
  deriving (Eq,Show)



type Stack = [Val]
type Domain = Stack -> Maybe Stack


-- Define the semantics of a StackLang command (ignore If at first).
cmd :: Stack_Cmd -> Domain
cmd (PushN i)    = \s -> Just (LeftI i : s)
cmd (PushB b)    = \s -> Just (RightB b : s)
cmd (PushS str)  = \s -> Just (MiddleS str : s)
cmd Add          = \s -> case s of
                           (LeftI i : LeftI j : s') -> Just (LeftI (i+j) : s')
                           (MiddleS x : MiddleS y : s') -> Just (MiddleS (y++x) : s')
                           (V (n, LeftI i) : LeftI j : s') -> Just (LeftI (i+j) : V (n, LeftI j) : s')
                           (LeftI i : V (n, LeftI j) : s') -> Just (V (n, LeftI (i+j)) : s')
                           (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (V (n, LeftI (i)) : V (x, LeftI (i+j)) : s')
                           (V (n, MiddleS i) : MiddleS j : s') -> Just (MiddleS (j++i) : V (n, MiddleS j) : s')
                           (MiddleS i : V (n, MiddleS j) : s') -> Just (V (n, MiddleS (j++i)) : s')
                           (V (n, MiddleS i) : V (x, MiddleS j) : s') -> Just (V (n, MiddleS (i)) : V (x, MiddleS (j++i)) : s')
                           _ -> Nothing
cmd Sub          = \s -> case s of
                           (LeftI i : LeftI j : s') -> Just (LeftI (j-i) : s')
                           (V (n, LeftI i) : LeftI j : s') -> Just (LeftI (j-i) : V (n, LeftI i) : s')
                           (LeftI i : V (n, LeftI j) : s') -> Just (V (n, LeftI (j-i)) : s')
                           (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (V (n, LeftI (i)) : V (x, LeftI (j-i)) : s')
                           _ -> Nothing
cmd Mul          = \s -> case s of
                           (LeftI i : LeftI j : s') -> Just (LeftI (j*i) : s')
                           (V (n, LeftI i) : LeftI j : s') -> Just (LeftI (j*i) : V (n, LeftI i) : s')
                           (LeftI i : V (n, LeftI j) : s') -> Just (V (n, LeftI (j*i)) : s')
                           (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (V (n, LeftI (i)) : V (x, LeftI (j*i)) : s')
                           _ -> Nothing
cmd Equ          = \s -> case s of
                           (LeftI i  : LeftI j  : s') -> Just (RightB (i == j) : s')
                           (MiddleS x : MiddleS y : s') -> Just (RightB (x == y) : s')
                           (RightB a : RightB b : s') -> Just (RightB (a == b) : s')
                           (V (n, LeftI i) : LeftI j : s') -> Just (RightB (i == j) : V (n, LeftI i) : s')
                           (LeftI i : V (n, LeftI j) : s') -> Just (RightB (i == j) : V (n, LeftI j) : s')
                           (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (RightB (i == j) : V (n, LeftI (i)) : V (x, LeftI j) : s')
                           (V (n, MiddleS i) : MiddleS j : s') -> Just (RightB (i == j) : V (n, MiddleS i) : s')
                           (MiddleS i : V (n, MiddleS j) : s') -> Just (RightB (i == j) : V (n, MiddleS j) : s')
                           (V (n, MiddleS i) : V (x, MiddleS j) : s') -> Just (RightB (i == j) : V (n, MiddleS (i)) : V (x, MiddleS j) : s')
                           _ -> Nothing
cmd Larger       = \s -> case s of
                           (LeftI i  : LeftI j  : s') -> Just (RightB (i < j) : s')
                           (LeftI i : V (n, LeftI j) : s') -> Just (RightB (i < j) : V (n, LeftI j) : s')
                           (V (n, LeftI i) : LeftI j : s') -> Just (RightB (i < j) : V (n, LeftI i) : s')
                           (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (RightB (i < j) : V (n, LeftI (i)) : V (x, LeftI j) : s')
                           _ -> Nothing
cmd Smaller      = \s -> case s of
                           (LeftI i  : LeftI j  : s') -> Just (RightB (i > j) : s')
                           (LeftI i : V (n, LeftI j) : s') -> Just (RightB (i > j) : V (n, LeftI j) : s')
                           (V (n, LeftI i) : LeftI j : s') -> Just (RightB (i > j) : V (n, LeftI i) : s')
                           (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (RightB (i > j) : V (n, LeftI (i)) : V (x, LeftI j) : s')
                           _ -> Nothing
cmd (IfElse t e) = \s -> case s of
                           (RightB True  : s') -> prog t s'
                           (RightB False : s') -> prog e s'
                           _ -> Nothing
cmd (Loop c r)   = \s -> case s of -- it's while loop (not do-while)
                           (LeftI i : s') -> if (i == 0) then Just s' else Nothing -- Infinity loop
                           (MiddleS i : s') -> Nothing -- Infinity loop
                           (RightB True : s') -> Nothing -- Infinity loop
                           (RightB False : s') -> Just s'
                           (V (n, MiddleS _) : s') -> Nothing -- String type can't be in condition.
                           (V (n, RightB True) : s') -> Nothing
                           (V (n, RightB False) : s') -> Just s'
                           (V (n, LeftI v) : s') -> loop (V (n, LeftI v))  c  r  s'
                           _ -> Nothing
cmd Dup         = \s -> case s of
                           (LeftI i : s') -> Just (LeftI i : LeftI i : s')
                           (MiddleS x : s') -> Just (MiddleS x : MiddleS x : s')
                           (RightB a : s') -> Just (RightB a : RightB a : s')              
                           _ -> Nothing
cmd Drop         = \s -> case s of
                           ( _ : s') -> Just (s')             
                           _ -> Nothing
cmd Swap         = \s -> case s of
                           (a  : b  : s') -> Just (b  : a : s')   
                           _ -> Nothing
cmd Over     = \s -> case s of
                           (x : y : s') -> Just (x : y : x : s')
                           _ -> Nothing
cmd Rot     = \s -> case s of
                           (x : y : z : s') -> Just (y : z : x : s')
                           _ -> Nothing
cmd (Let n) = \s -> if (cmd (Ref n) s) == Nothing then case s of -- Avoid duplicates varname.
                            (LeftI i : s') -> Just (V (n, LeftI i) : s')
                            (MiddleS str : s') -> Just (V (n, MiddleS str) : s')
                            (RightB b : s') -> Just (V (n, RightB b) : s')
                            _ -> Nothing
                    else Nothing
cmd (Ref n) = \s -> case reverse s of -- reverse하고나서 다시 reverse했을때 스택이 그대로인지 확인필요!!!!!
                    (V (name, value) : s') -> if (name == n) then Just (value : (reverse (V (name, value) : s'))) else if (find n s') == FError then Nothing else Just ((find n s') : reverse s')
                    _ -> Nothing

-- helper function for Ref
find :: Varname -> Stack -> Val
find n (s:s') = case s of
                V (name, v) -> if (name == n) then v else find n s'
                _ -> FError

-- Define the semantics
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
loop :: Val -> Prog -> Prog -> Stack -> Maybe Stack
loop (V (n, LeftI v)) c r s' = case run ((PushN v):c) of
                            Just [RightB True] -> Just (V (n, LeftI v) : s')
                            Just [RightB False] -> case (loophelp (V (n, LeftI v)) r) of
                                                    V (n, v') -> loop (V (n, v')) c r s'
                                                    FError -> Nothing
                            _ -> Nothing

loophelp :: Val -> Prog -> Val
loophelp (V (n, LeftI v)) r = case run ((PushN v):r) of
                    Just [LeftI b] -> V (n, LeftI b)
                    _ -> FError



exloop :: Prog
exloop = [PushN 2, Let("Test"), Loop [PushN 5, Larger] [PushN 1, Add]]

-- 4. Procedures/functions with arguments (or some other abstraction mechanism).
--    You should provide a way to factor out repeated code and give it a name so that it can be reused. 
--    For imperative/functional languages, you must decide what kind of parameter passing scheme to use, which we’ll discuss in class. (Passing arguments is trivial for stack-based languages since arguments are passed on the stack!).
extest :: Prog
extest = [PushN 3, Let("Test"), PushN 4, Let("Test2"), Ref("Test"), PushN 3, Add, PushN 6, Equ]
extest2 :: Prog
extest2 = [PushN 3, Let("Test"), PushN 4, Let("Test2")]
extest3 :: Prog
extest3 = [PushN 3, Let("Test"), PushN 4, Let("Test2"), Smaller, Dup]

-- 5. Stack manipulation operations (stack-based languages only). 
--    You should provide a set of basic operations for manipulating values on the stack. You may want to look at a set of Forth stack maneuvers for inspiration. -}



{-Additionally, you must include at least 3 points worth of the following features. The point value of each feature is indicated in parentheses after the feature name.
1. Strings and operations (1).
    This feature would enable creating and manipulating string values, such as “hello world!”. The set of operations is up to you, but should must include at least concatenation.

-- run [PushS "hello ", PushS "world", PushS "!", Add, Add]
-- => Just [MiddleS "!worldhello"]
-- Need a function to print String Correctly.

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