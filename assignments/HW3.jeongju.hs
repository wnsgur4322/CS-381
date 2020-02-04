-- Team members
-- Junhyeok Jeong, jeongju@oregonstate.edu
-- Youngjoo Lee, leey3@oregonstate.edu
module HW3 where
import Data.List
import Prelude hiding (Num,Var,Macro)

-- Tasks
-- mini logo grammar
-- 1. Define the abstract syntax of MiniLogo as a set of Haskell data types.
-- You should use built-in types for num, var, and macro. (If you want to define a type Num, you will have to hide that name from the Prelude).

-- The concrete syntax of the MiniLogo language is defined by the following grammar:

--num	::=	(any natural number)	
--var	::=	(any variable name)	
--macro	::=	(any macro name)	

--prog	::=	ε   |   cmd ; prog	sequence of commands

--mode	::=	down   |   up	pen status

--expr	::=	var	variable reference
-- |	num	literal number
-- |	expr + expr	addition expression

--cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro

type Num = Int                                 -- (any natural number)
type Var = String                              -- (any variable name)	
type Macro = String                            -- (any macro name)	

type Prog = [Cmd]                         -- sequence of commands = list of Commands

data Mode = Down | Up
            deriving (Show, Eq)                -- pen status

data Expr = Ref Var                             -- variable reference
            | Num Int                            -- literal number
            | Expr `Add` Expr
            deriving (Show, Eq)                  -- addition expression

data Cmd = Pen Mode                        -- change pen mode
            | Move ( Expr , Expr )              -- move pen to a new position
            | Define Macro [Var] Prog           -- define a macro
            | Call Macro [Expr]                 -- invoke a macro
            deriving (Show, Eq)

-- 2. Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas) draws a line segment from (x1,y1) to (x2,y2).
--      First, write the macro in MiniLogo concrete syntax (i.e. the notation defined by the grammar and used in the example programs above).
--      Include this definition in a comment in your submission.
--      Second, encode the macro definition as a Haskell value using the data types defined in Task 1.
-- This corresponds to the abstract syntax of MiniLogo. Your Haskell definition should start with something like line = Define "line" ...

-- the concrete syntax
-- Define line (x1, y1, x2, y2) {     -- takes two points (x1, y1) and (x2, y2) because the 'line' function required to draw a line from point A to point B
                                      -- all Var x1,y2,x2,y2 are Strings, so put " "
--    Pen Up, Move (x1, y1)                         -- 1. update Pen status and holding Pen on point A position before draw
--    Pen Down Move (x2, y2)          -- 2. update Pen status and then move to destination point B.
-- }
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]      -- | Define Macro [Var]
    [Pen Up, Move (Ref "x1", Ref "y1"), Pen Down, Move (Ref "x2", Ref "y2")]    -- {Pen Mode, Move (Expr = Ref Var, Expr = Ref Var), Pen Down (Expr = Ref Var, Expr = Ref Var)}

-- 3. Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) that draws a big “X” of width w and height h, starting from position (x,y).
-- Your definition should not contain any move commands.
--      First, write the macro in MiniLogo concrete syntax and include this definition in a comment in your submission.
--      Second, encode the macro definition as a Haskell value, representing the abstract syntax of the definition.

-- Concrete Syntax for nix:
-- define nix (x, y, w, h) {    -- Takes two points(x, y), width(w), and height(h) because the 'nix' function required to draw a bix "X" that has width(w) and height(h).
                                -- all Var x1,y2,x2,y2 are Strings, so put " "
-- line (x, y, x+w, y+h),       -- 1. draw a line from (x, y) to (x+w, y+h) with "line"
-- line(x, y+h, x+w, y)         -- 2. draw a line from (x, y+h) to (x+w, h) with "line" 
--}                             -- No needs Pen status change because "line" has it's own pen status change.
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]     -- | Define Macro [Var]
    [Call "line" [(Ref "x"), (Ref "y"), Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")],
    Call "line"  [(Ref "x"), Add (Ref "y") (Ref "h"), Add (Ref "x") (Ref "w"), (Ref "y")]]
-- {Call Macro(Expr = Ref Var, Expr = Ref Var, Expr = Ref Var `Add` Ref Var, Expr = Ref Var `Add` Ref Var), 
-- Call Macro(Expr = Ref Var, Expr = Ref Var `Add` Ref Var, Expr = Ref Var `Add` Ref Var, Expr = Ref Var)}


-- 4. Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program that draws a staircase of n steps starting from (0,0).
-- Below is a visual illustration of what the generated program should draw for a couple different applications of steps. You may assume that n ≥ 0.
steps :: Int -> Prog
steps 0 = []
steps n = 
-- 5. Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names of all of the macros that are defined anywhere in a given MiniLogo program.
-- Don’t worry about duplicates—if a macro is defined more than once, the resulting list may include multiple copies of its name.

-- 6. Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program.
-- That is, it transforms the abstract syntax (a Haskell value) into nicely formatted concrete syntax (a string of characters).
-- Your pretty-printed program should look similar to the example programs given above; however, for simplicity you will probably want to print just one command per line.
-- In GHCi, you can render a string with newlines by applying the function putStrLn. So, to pretty-print a program p use: putStrLn (pretty p).

-- Bonus Problems
-- These problems are not any harder than the other problems in the assignment.
-- They are included mainly to give you a bit more practice writing Haskell functions that manipulate syntax, if you want that.
-- However, as a little external incentive, you will earn a small amount of extra credit if you complete them both.

-- 7. Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by replacing any additions of literals with the result. 
-- For example, given the expression (2+3)+x, optE should return the expression 5+x.

-- 8.Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions contained in a given program using optE.