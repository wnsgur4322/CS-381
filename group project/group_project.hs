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
type Var = (Varname, Val)

data Four_Cmd = PushN Int
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
         | Bind (Varname, Four_Cmd)
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
   | RightB Bool               -- Bool
   | MiddleS String            -- String
   | V Var                     -- Var
   | FError                    -- Error
  deriving (Eq,Show)



type Stack = [Val]
type Domain = Stack -> Maybe Stack

-- Define the semantics of a StackLang command (ignore If at first).
cmd :: Four_Cmd -> Domain
cmd (PushN i)    = \s -> Just (LeftI i : s)
cmd (PushB b)    = \s -> Just (RightB b : s)
cmd (PushS str)  = \s -> Just (MiddleS str : s)
cmd Add          = \s -> add_helper s
cmd Sub          = \s -> sub_helper s
cmd Mul          = \s -> mul_helper s
cmd Equ          = \s -> equ_helper s
cmd Larger       = \s -> larger_helper s
cmd Smaller      = \s -> smaller_helper s
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
cmd Dup         = \s -> dup_helper s
cmd Drop         = \s -> drop_helper s
cmd Swap         = \s -> swap_helper s
cmd Over     = \s -> over_helper s
cmd Rot     = \s -> rot_helper s
cmd (Let n) = \s -> case ((cmd (Ref n) s), s) of -- Avoid duplicates varname.
                          (Nothing, (LeftI i : s')) -> Just (V (n, LeftI i) : s')
                          (Nothing, (MiddleS str : s')) -> Just (V (n, MiddleS str) : s')
                          (Nothing, (RightB b : s')) -> Just (V (n, RightB b) : s')
                          _ -> Nothing
cmd (Ref n) = \s -> case s of -- if you call 'reverse' again after it called once, the user should check the stack hasn't been changed
                    (V (name, value) : s') -> if (name == n) then Just (value : V (name, value) : s') else if (find n s') == FError then Nothing else Just ((find n s) : s)
                    [] -> Nothing
                    _ -> if (find n s) == FError then Nothing else Just ((find n s) : s)
cmd (Bind (n, v)) = \s -> case v of 
                            (Ref t) -> case ((cmd (Ref n) s), (cmd (Ref t) s)) of
                                        (Just (LeftI i:s'), Just (LeftI j:x')) -> if (findVar (n, LeftI j) (reverse s)) /= [FError] then Just (findVar (n, LeftI j) (reverse s)) else Nothing
                                        (Just (MiddleS i:s'), Just (MiddleS j:x')) -> if (findVar (n, MiddleS j) (reverse s)) /= [FError] then Just (findVar (n, MiddleS j) (reverse s)) else Nothing
                                        (Just (RightB i:s'), Just (RightB j:x')) -> if (findVar (n, RightB j) (reverse s)) /= [FError] then Just (findVar (n, RightB j) (reverse s)) else Nothing
                                        _ -> Nothing
                            _ -> case ((cmd (Ref n) s), (cmd v [])) of
                                    (Just (LeftI i:s'), Just [LeftI j]) -> if (findVar (n, LeftI j) (reverse s)) /= [FError] then Just (findVar (n, LeftI j) (reverse s)) else Nothing
                                    (Just (MiddleS i:s'), Just [MiddleS j]) -> if (findVar (n, MiddleS j) (reverse s)) /= [FError] then Just (findVar (n, MiddleS j) (reverse s)) else Nothing
                                    (Just (RightB i:s'), Just [RightB j]) -> if (findVar (n, RightB j) (reverse s)) /= [FError] then Just (findVar (n, RightB j) (reverse s)) else Nothing
                                    _ -> Nothing


-- Helpers to make neat 'cmd'

add_helper :: Stack -> Maybe Stack
add_helper = \s -> case s of
                (LeftI i : LeftI j : s') -> Just (LeftI (i+j) : s')
                (MiddleS x : MiddleS y : s') -> Just (MiddleS (y++x) : s')
                (V (n, LeftI i) : LeftI j : s') -> Just (LeftI (i+j) : V (n, LeftI i) : s')
                (LeftI i : V (n, LeftI j) : s') -> Just (V (n, LeftI (i+j)) : s')
                (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (V (n, LeftI (i)) : V (x, LeftI (i+j)) : s')
                (V (n, MiddleS i) : MiddleS j : s') -> Just (MiddleS (j++i) : V (n, MiddleS j) : s')
                (MiddleS i : V (n, MiddleS j) : s') -> Just (V (n, MiddleS (j++i)) : s')
                (V (n, MiddleS i) : V (x, MiddleS j) : s') -> Just (V (n, MiddleS (i)) : V (x, MiddleS (j++i)) : s')
                _ -> Nothing

sub_helper :: Stack -> Maybe Stack
sub_helper = \s -> case s of
                (LeftI i : LeftI j : s') -> Just (LeftI (j-i) : s')
                (V (n, LeftI i) : LeftI j : s') -> Just (LeftI (j-i) : V (n, LeftI i) : s')
                (LeftI i : V (n, LeftI j) : s') -> Just (V (n, LeftI (j-i)) : s')
                (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (V (n, LeftI (i)) : V (x, LeftI (j-i)) : s')
                _ -> Nothing

mul_helper :: Stack -> Maybe Stack
mul_helper = \s -> case s of
                (LeftI i : LeftI j : s') -> Just (LeftI (j*i) : s')
                (V (n, LeftI i) : LeftI j : s') -> Just (LeftI (j*i) : V (n, LeftI i) : s')
                (LeftI i : V (n, LeftI j) : s') -> Just (V (n, LeftI (j*i)) : s')
                (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (V (n, LeftI (i)) : V (x, LeftI (j*i)) : s')
                _ -> Nothing

equ_helper :: Stack -> Maybe Stack
equ_helper = \s -> case s of
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

larger_helper :: Stack -> Maybe Stack
larger_helper = \s -> case s of
                    (LeftI i  : LeftI j  : s') -> Just (RightB (i < j) : s')
                    (LeftI i : V (n, LeftI j) : s') -> Just (RightB (i < j) : V (n, LeftI j) : s')
                    (V (n, LeftI i) : LeftI j : s') -> Just (RightB (i < j) : V (n, LeftI i) : s')
                    (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (RightB (i < j) : V (n, LeftI (i)) : V (x, LeftI j) : s')
                    _ -> Nothing

smaller_helper :: Stack -> Maybe Stack
smaller_helper = \s -> case s of
                    (LeftI i  : LeftI j  : s') -> Just (RightB (i > j) : s')
                    (LeftI i : V (n, LeftI j) : s') -> Just (RightB (i > j) : V (n, LeftI j) : s')
                    (V (n, LeftI i) : LeftI j : s') -> Just (RightB (i > j) : V (n, LeftI i) : s')
                    (V (n, LeftI i) : V (x, LeftI j) : s') -> Just (RightB (i > j) : V (n, LeftI (i)) : V (x, LeftI j) : s')
                    _ -> Nothing

dup_helper :: Stack -> Maybe Stack
dup_helper = \s -> case s of
                    (LeftI i : s') -> Just (LeftI i : LeftI i : s')
                    (MiddleS x : s') -> Just (MiddleS x : MiddleS x : s')
                    (RightB a : s') -> Just (RightB a : RightB a : s')              
                    _ -> Nothing

drop_helper :: Stack -> Maybe Stack
drop_helper = \s -> case s of
                    ( _ : s') -> Just (s')             
                    _ -> Nothing

swap_helper :: Stack -> Maybe Stack
swap_helper = \s -> case s of
                    (a  : b  : s') -> Just (b  : a : s')   
                    _ -> Nothing

over_helper :: Stack -> Maybe Stack
over_helper = \s -> case s of
                    (x : y : s') -> Just (x : y : x : s')
                    _ -> Nothing

rot_helper :: Stack -> Maybe Stack
rot_helper = \s -> case s of
                    (x : y : z : s') -> Just (y : z : x : s')
                    _ -> Nothing




bindtest :: Prog
bindtest = [PushN 3, Let("a"), PushN 4, Let("b"), Ref("a"), Ref("b"), Bind(("a", Ref("b"))), Ref("a")]
bindtest2 :: Prog
bindtest2 = [PushN 3, Let("a"), PushN 4, Let("b"), Ref("b"), Ref("a")]
bindtest3 :: Prog
bindtest3 = [PushN 3, Let("a"), PushN 4, Let("b"), Ref("a"), Ref("b"), Bind(("a", PushN 4)), Ref("a")]
-- Result: Just [LeftI 4,LeftI 3,LeftI 4,V ("n",LeftI 4)]
{-
cmd (Bind (n, v)) = \s -> if (cmd (Ref n) s ) /= Nothing then case reverse s of
                            (V (name, LeftI value) : s') -> if (name == n) then case cmd v [] of
                                                                            Just [LeftI i] -> Just (reverse (V (name, LeftI i) : s')) 
                                                                            _ -> Nothing
                                                            else if (find n s') == FError then Nothing 
                                                            else Just (findVar (n, v) s)

                            (V (name, MiddleS value) : s') -> if (name == n) then case cmd v [] of
                                                                            Just [MiddleS i] -> Just (reverse (V (name, MiddleS i) : s')) 
                                                                            _ -> Nothing
                                                            else if (find n s') == FError then Nothing 
                                                            else Just (findVar (n, v) s)
                            (V (name, RightB value) : s') -> if (name == n) then case cmd v [] of
                                                                            Just [RightB i] -> Just (reverse (V (name, RightB i) : s')) 
                                                                            _ -> Nothing
                                                            else if (find n s') == FError then Nothing 
                                                            else Just (findVar (n, v) s)
                            _ -> Nothing
                          else Nothing
-}

-- helper function for Ref
find :: Varname -> Stack -> Val
find n (s:s') = case s of
                V (name, v) -> if (name == n) then v else if s' == [] then FError else find n s'
                _ -> if s' == [] then FError else find n s'

findVar :: Var -> Stack -> Stack
findVar (n, v) (s:s') = case s of
                        (V (name, value)) -> if (name == n) then (reverse (V (name, v) : s')) 
                                             else ((findVar (n, v) s') ++ [s])
                        _ -> if s' == [] then [FError] else ((findVar (n, v) s') ++ [s])

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
-- cmd (IfElse t e) = \s -> case s of
--                            (RightB True  : s') -> prog t s'
--                            (RightB False : s') -> prog e s'
--                            _ -> Nothing


-- 3. Recursion/loops. 
--    You should provide some way to loop in your language, either through an explicit looping construct (e.g. while) or through recursive functions.
-- Loops (While loop) for Integers.
loop :: Val -> Prog -> Prog -> Stack -> Maybe Stack
loop (V (n, LeftI v)) c r s' = case run ((PushN v):c) of
                            Just [RightB True] -> Just (V (n, LeftI v) : s')
                            Just [RightB False] -> case (loophelp (V (n, LeftI v)) r s') of
                                                    [V (n, v')] -> loop (V (n, v')) c r s'
                                                    (V (n, v'):x') -> loop (V (n, v')) c r (x')
                                                    [FError] -> Nothing
                            _ -> Nothing

loophelp :: Val -> Prog -> Stack -> Stack
loophelp (V (n, LeftI v)) r s = case prog ((PushN v):r) s of
                    Just [LeftI b] -> [V (n, LeftI b)]
                    Just (LeftI b:s') -> (V (n, LeftI b):s')
                    _ -> [FError]


exloop :: Prog
exloop = [PushN 4, Let("t"), PushN 3, Let("Test"), Loop [PushN 5, Larger] [PushN 1, Add]]

-- example of language usage: make Fibonacci numbers function with 'Four'
-- python ver.
{-
def fibonacci(n):
    a = 0
    b = 1
    for i in range(0, n):
        temp = a
        a = b
        b = temp + b
    return a

-- c++ ver.
def cpp_fib(n){
    int a,b = 0,1;
    int temp;
    for (int i =0; i<n; i++;){
        temp = a;
        a = b;
        b = temp +b;
    }
    return a
}
-}
-- recursive fibonacci by original haskell
has_fib :: Int -> Int
has_fib 0 = 0
has_fib 1 = 1
has_fib n = has_fib (n-1) + has_fib (n-2)

-- recursive fibonacci numbers by 'Four' language
rec_fib :: Int -> Prog
rec_fib 0 = [PushN 0]
rec_fib 1 = [PushN 0, PushN 1, Add]
rec_fib n = rec_fib (n-1) ++ rec_fib(n-2) ++ [Add]

-- iterative fibonacci umbers by 'Four' language
itr_fib :: Int -> Prog
itr_fib n = [PushN 0, Let("a"), PushN 1, Let("b"), PushN 0, Let("temp"), PushN 1, Let("i"), 
            Loop [PushN n, Larger] [PushN 1, Add, Bind("temp", Ref ("a")), Bind("a", Ref ("b")), Ref ("temp"), Ref ("b"), Add, Let("c"), Bind("b",Ref("c")), Drop], Drop, Drop, Drop]

-- example of language usage: make factorial function with 'Four'
-- python ver.
{-
def factorial(n):
    fact = 1
    for i in range(1, n):
        fact = fact * i
        
    return fact

-- c++ ver.
def cpp_factorial(n){
    int fact = 1;
    for (int i =1; i<n; i++;){
        fact = fact * i;
    }
    return fact
}
-}
factorial :: Int -> Prog
factorial n = [PushN 1, Let("fact"), PushN 1, Let("temp"),
              PushN 1, Let("i"), Loop [PushN n, Larger] [PushN 1, Add, Bind("temp", Ref("i")), Ref ("fact"), Ref ("temp"), Mul, Let("mul"), Bind("fact", Ref ("mul")), Drop]]

factorial2 :: Int -> Prog
factorial2 n = [PushN 1, Let("fact"), PushN 1, Let ("temp"),
              PushN 1, Let("i"), Loop [PushN n, Larger] [PushN 1, Add, Ref ("temp"), PushN 1, Add, Let ("temp2"), Bind("temp", Ref ("temp2")), Drop, Ref ("fact"), Ref ("temp"), Mul, Let("mul"), Bind("fact", Ref ("mul")), Drop]]

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
-- Fixed!
-- ex)
-- run [PushS "String ", PushS "Test",  Add, PushS "Correct", Add]
-- Just [MiddleS "String TestCorrect"]

2. Tuples and operations (1).
    This feature would enable creating tuples of other values, such as (2,true). 
    You should also be able to represent tuples containing tuples, such as (2,(true,"hello")).
    The set of operations is up to you but must include at least operations to get the first and second elements from the tuple. -}

{-
3. List/array data type and operations (2). 
    This feature would enable creating and manipulating lists of values. Your operations should include standard operations such as indexing and (for lists) concatenation. 
    Your language must also be able to process lists in some way, for example, by looping over them or through recursive pattern matching. -}
int_list :: [Int] -> Prog
int_list (x:xs) = map PushN (x:xs)

list_concatenation :: [Int] -> [Int] -> Prog
list_concatenation (x:xs) (y:ys) = int_list (x:xs) ++ int_list (y:ys)

{-
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
-}
data Type = TBool | FBool | TInt | TString | T (String, Type) | TError
  deriving (Eq,Show)
type Tstack = [Type]

typeOf :: Four_Cmd -> Tstack -> [Type]
typeOf (PushN i)    = \s -> (TInt : s)
typeOf (PushB b)    = \s -> if b == True then (TBool : s) else (FBool : s)
typeOf (PushS s)    = \s -> (TString : s)
typeOf Add          = \s -> tadd_helper s
typeOf Sub          = \s -> tsub_helper s
typeOf Mul          = \s -> tmul_helper s
typeOf Equ          = \s -> tequ_helper s
typeOf Larger       = \s -> tlarger_helper s
typeOf Smaller      = \s -> tsmaller_helper s
typeOf (Let n)      = \s -> case ((typeOf (Ref n) s), s) of
                      ([TError], (TInt : s')) -> (T (n, TInt) : s')
                      ([TError], (TString : s')) -> (T (n, TString) : s')
                      ([TError], (TBool : s')) -> (T (n, TBool) : s')
                      ([TError], (FBool : s')) -> (T (n, FBool) : s')
                      _ -> [TError]
typeOf (Ref n)      = \s -> case s of
                        (T (name, v) : s') -> if (name == n) then (v : T (name, v) : s') else if (findType n s') == TError then [TError] else ((findType n s') : s)
                        [] -> [TError]
                        _ -> if (findType n s) == TError then [TError] else ((findType n s) : s)
typeOf (IfElse t e) = \s -> case s of
                      (TBool: s') -> case (typeprog t s', typeprog e s') of
                                     (tt, te) -> if tt == te then if (tt /= [TError]) then (tt ++ s') else [TError] else [TError]
                      _ -> [TError]
typeOf (Loop c r) = \s -> case s of
                        (TInt : s') -> case (typeprog c s', typeprog r s') of
                            (TBool : _ , x : _) -> (x : s')
                            (T (n, TBool) : _ , x : _) -> (x : s')
                            (FBool : _ , x : _) -> (x : s')
                            (T (n, FBool) : _ , x : _) -> (x : s')
                            _ -> [TError]
                        (T (_, TInt) : s') -> case (typeprog c s', typeprog r s') of
                            (TBool : _ , x : _) -> (x : s')
                            (T (n, TBool) : _ , x : _) -> (x : s')
                            (FBool : _ , x : _) -> (x : s')
                            (T (n, FBool) : _ , x : _) -> (x : s')
                            _ -> [TError]
                        (TBool : s') -> [TError]
                        (T (n, TBool) : s') -> [TError]
                        (FBool : s') -> typeprog r s'
                        (T (n, FBool) : s') -> typeprog r s'
                        _ -> [TError]
typeOf Dup        = \s -> tdup_helper s    
typeOf Drop         = \s -> tdrop_helper s
typeOf Swap         = \s -> tswap_helper s
typeOf Over     = \s -> tover_helper s
typeOf Rot     = \s -> trot_helper s
typeOf (Bind (n, v)) = \s -> case v of
                                (Ref t) -> case ((typeOf (Ref n) s), (typeOf (Ref t) s)) of
                                                ((TInt:s'), (TInt:x')) -> findTypeVar (T (n, TInt)) (reverse s)
                                                ((TString:s'), (TString:x')) -> findTypeVar (T (n, TString)) (reverse s)
                                                ((TBool:s'), (TBool:x')) -> findTypeVar (T (n, TBool)) (reverse s)
                                                ((TBool:s'), (FBool:x')) -> findTypeVar (T (n, FBool)) (reverse s)
                                                ((FBool:s'), (FBool:x')) -> findTypeVar (T (n, FBool)) (reverse s)
                                                ((FBool:s'), (TBool:x')) -> findTypeVar (T (n, TBool)) (reverse s)
                                                _ -> [TError]   
                                _ -> case ((typeOf (Ref n) s), (typeOf v [])) of
                                                ((TInt:s'), [TInt]) -> findTypeVar (T (n, TInt)) (reverse s)
                                                ((TString:s'), [TString]) -> findTypeVar (T (n, TString)) (reverse s)
                                                ((TBool:s'), [TBool]) -> findTypeVar (T (n, TBool)) (reverse s)
                                                ((TBool:s'), [FBool]) -> findTypeVar (T (n, FBool)) (reverse s)
                                                ((FBool:s'), [FBool]) -> findTypeVar (T (n, FBool)) (reverse s)
                                                ((FBool:s'), [TBool]) -> findTypeVar (T (n, TBool)) (reverse s)
                                                _ -> [TError]

-- typeof helpers to make neat code
tadd_helper :: Tstack -> [Type]
tadd_helper = \s -> case s of
                      (TInt : TInt : s') -> (TInt : s')
                      (TString : TString : s') -> (TString : s')
                      (T (n, TInt) : TInt : s') -> (TInt : T (n, TInt) : s')
                      (TInt : T (n, TInt) : s') -> (T (n, TInt) : s')
                      (T (n, TInt) : T (x, TInt) : s') -> (T (n, TInt) : T (x, TInt) : s')
                      (T (n, TString) : TString : s') -> (TString : T (n, TString) : s')
                      (TString : T (n, TString) : s') -> (T (n, TString) : s')
                      (T (n, TString) : T (x, TString) : s') -> (T (n, TString) : T (x, TString) : s')
                      _ -> [TError]

tsub_helper :: Tstack -> [Type]
tsub_helper = \s -> case s of
                      (TInt : TInt : s') -> (TInt : s')
                      (T (n, TInt) : TInt : s') -> (TInt : T (n, TInt) : s')
                      (TInt : T (n, TInt) : s') -> (T (n, TInt) : s')
                      (T (n, TInt) : T (x, TInt) : s') -> (T (n, TInt) : T (x, TInt) : s')    
                      _ -> [TError]

tmul_helper :: Tstack -> [Type]
tmul_helper = \s -> case s of
                      (TInt : TInt : s') -> (TInt : s')
                      (T (n, TInt) : TInt : s') -> (TInt : T (n, TInt) : s')
                      (TInt : T (n, TInt) : s') -> (T (n, TInt) : s')
                      (T (n, TInt) : T (x, TInt) : s') -> (T (n, TInt) : T (x, TInt) : s')    
                      _ -> [TError]

tequ_helper :: Tstack -> [Type]
tequ_helper = \s -> case s of
                      (TInt : TInt : s') -> (FBool : s')
                      (TBool : TBool : s') -> (TBool : s')
                      (TBool : FBool : s') -> (FBool : s')
                      (FBool : TBool : s') -> (FBool : s')
                      (FBool : FBool : s') -> (FBool : s')
                      (TString : TString : s') -> (FBool : s')
                      (T (n, TInt) : TInt : s') -> (FBool : T (n, TInt) : s')
                      (TInt : T (n, TInt) : s') -> (FBool : T (n, TInt) : s')
                      (T (n, TInt) : T (x, TInt) : s') -> (FBool : T (n, TInt) : T (x, TInt) : s')
                      (T (n, TString) : TString : s') -> (FBool : T (n, TString) : s')
                      (TString : T (n, TString) : s') -> (FBool : T (n, TString) : s')
                      (T (n, TString) : T (x, TString) : s') -> (FBool : T (n, TString) : T (x, TString) : s')
                      _ -> [TError]

tlarger_helper :: Tstack -> [Type]
tlarger_helper = \s -> case s of
                           (TInt  : TInt  : s') -> (FBool : s')
                           (T (n, TInt) : TInt : s') -> (FBool : T (n, TInt) : s')
                           (TInt : T (n, TInt) : s') -> (FBool : T (n, TInt) : s')
                           (T (n, TInt) : T (x, TInt) : s') -> (FBool : T (n, TInt) : T (x, TInt) : s')
                           _ -> [TError]

tsmaller_helper :: Tstack -> [Type]
tsmaller_helper = \s -> case s of
                           (TInt  : TInt  : s') -> (FBool : s')
                           (T (n, TInt) : TInt : s') -> (FBool : T (n, TInt) : s')
                           (TInt : T (n, TInt) : s') -> (FBool : T (n, TInt) : s')
                           (T (n, TInt) : T (x, TInt) : s') -> (FBool : T (n, TInt) : T (x, TInt) : s')
                           _ -> [TError]

tdup_helper :: Tstack -> [Type]
tdup_helper = \s -> case s of
                      (TInt : s') -> (TInt : s')
                      (TBool : s') -> (TBool : s')
                      (FBool : s') -> (FBool : s')
                      (TString : s') -> (TString : s')
                      _ -> [TError]

tdrop_helper :: Tstack -> [Type]
tdrop_helper = \s -> case s of
                           ( _ : s') -> s'             
                           _ -> [TError]

tswap_helper :: Tstack -> [Type]
tswap_helper = \s -> case s of
                           (a  : b  : s') -> (b : a : s')
                           _ -> [TError]

tover_helper :: Tstack -> [Type]
tover_helper = \s -> case s of
                           (x : y : s') -> (x : y : x : s')
                           _ -> [TError]

trot_helper :: Tstack -> [Type]
trot_helper = \s -> case s of
                           (x : y : z : s') -> (y : z : x : s')
                           _ -> [TError]

findTypeVar :: Type -> Tstack -> [Type]
findTypeVar (T (n, v)) (s:s') = case s of
                        (T (name, value)) -> if (name == n) then (reverse (T (name, v) : s')) 
                                             else ((findTypeVar (T (n, v)) s') ++ [s])
                        _ -> if s' == [] then [TError] else ((findTypeVar (T (n, v)) s') ++ [s])
findType :: String -> Tstack -> Type
findType n (s:s') = case s of
                    T (name, v) -> if (name == n ) then v else if s' == [] then TError else findType n s'
                    _ -> if s' == [] then TError else findType n s'

typeprog :: Prog -> Tstack -> [Type]
typeprog []    = \s -> if s == [TError] then error "type error occurs" else s
typeprog (c:p) = \s -> case typeOf c s of
                     [TError] -> error "type error occurs"
                     s' -> typeprog p s'


typeProgT :: Prog -> Tstack -> Bool
typeProgT []    = \s -> if s == [TError] then False else True
typeProgT (c:p) = \s -> case typeOf c s of
                     [TError] -> False
                     s' -> typeProgT p s'


runProg :: Prog -> Maybe Stack
runProg p = if typeProgT p [] then prog p [] else Nothing
{-
7. Input/output (2).
    This feature would enable reading and/or printing output from programs in your language. 
    This feature is tricky to implement in pure Haskell, but you can simulate it by extending your semantic domain with strings (or lists of strings) that represent input/output to/from your program.

8. Some other feature of your choice (?). 
    You’re free to pick some other feature I haven’t listed here! 
    In your milestone submission, be clear about what feature you’re picking and how many points you think it should be worth. We’ll negotiate from there, if needed. 
    (Minor constraint: You must have at least one 2-point or higher feature. 
    In other words, you can’t just add another simple type of value to a language with strings and tuples and call it a day!) -}
-- We can change a value which has a name by "Let" through "Bind" like a = 3; a = 5; => Result: a = 5.
-- Also, through "Bind", we can do not only a = 3; a = 5;, also a = 3; b = 5; a = b; => Result : a = 5, b = 5.