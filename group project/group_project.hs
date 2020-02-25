-- Team members
-- Junhyeok Jeong, jeongju@oregonstate.edu
-- Youngjoo Lee, leey3@oregonstate.edu
-- Ethan Mendelson, mendelse@oregonstate.edu

module Group_project where

{- Feature menu
When designing your language, you must include some version of all of the following features.
1. Basic data types and operations. 
    You should provide at least boolean values and integers; you may want to include floating point numbers as well.
    You will need a way to represent both literal values and operations on these types. -}

-- | A single register imperative language.
module While where

--
-- * Syntax
--

--  Before refactoring:
--
--    int  ::= (any integer)
--
--    expr ::= `R`                  -- load from register
--          |  int                  -- integer literal
--          |  expr `+` expr        -- addition expression
--          |  expr `≤` expr        -- less than or equal to
--
--    stmt ::= `R :=` expr          -- set register
--          |  `while` expr stmt    -- while loop
--          |  `begin` stmt* `end`  -- statement block
--

-- After refactoring to remove the possibility of type errors:

data Expr
   = Get
   | Lit Int
   | Add Expr Expr
  deriving (Eq,Show)

data Test
   = LTE Expr Expr
  deriving (Eq,Show)

data Stmt
   = Set Expr
   | While Test Stmt
   | Begin [Stmt]
  deriving (Eq,Show)


-- Example program:
--   begin
--     R := 1
--     while R <= 100
--       R := R + R
--   end

p :: Stmt
p = Begin
      [ Set (Lit 1)
      , While (LTE Get (Lit 100))
          (Set (Add Get Get))
      ]

--
-- * Semantics
--

type Reg = Int

-- Before refactoring:
--   expr: Reg -> Maybe (Either Int Bool)
--   stmt: Reg -> Maybe Reg
--
-- After refactoring:
--   expr: Reg -> Int
--   test: Reg -> Bool
--   stmt: Reg -> Reg

-- | Valuation function for expressions.
expr :: Expr -> Reg -> Int
expr Get       s = s
expr (Lit i)   s = i
expr (Add l r) s = expr l s + expr r s

-- | Valuation function for tests.
test :: Test -> Reg -> Bool
test (LTE l r) s = expr l s <= expr r s

-- | Valuation function for statements.
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     s = expr e s
stmt (While c b) s = if test c s then stmt (While c b) (stmt b s) else s
stmt (Begin ss)  s = stmts ss s  -- foldl (flip stmt) s ss
  where
    stmts []     r = r
    stmts (s:ss) r = stmts ss (stmt s r)


{- 2. Conditionals.
    You should provide some way to branch in your language (e.g. if-then-else).
3. Recursion/loops. 
    You should provide some way to loop in your language, either through an explicit looping construct (e.g. while) or through recursive functions.
4. Variables/local names (imperative/functional languages only).
    You should provide a way to give names to values in your language. 
    This might be mutable variables in an imperative language, or immutable local variables in a functional language.
5. Procedures/functions with arguments (or some other abstraction mechanism).
    You should provide a way to factor out repeated code and give it a name so that it can be reused. 
    For imperative/functional languages, you must decide what kind of parameter passing scheme to use, which we’ll discuss in class. (Passing arguments is trivial for stack-based languages since arguments are passed on the stack!).
6. Stack manipulation operations (stack-based languages only). 
    You should provide a set of basic operations for manipulating values on the stack. You may want to look at a set of Forth stack maneuvers for inspiration. -}

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