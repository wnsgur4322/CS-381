module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)                                          -- cmd takes 'Cmd' and 'State' data defined from MiniMiniLogo.hs
cmd (Pen Up) (mode, point) = ((Up, point), Nothing)                                 -- if takes 'Pen Up', then Keeps 'Up' and the point and output 'Nothing'
cmd (Pen Down) (mode, point) = ((Down, point), Nothing)                             -- if takes 'Pen Down', then Keeps 'Down' and the point and output 'Nothing'
cmd (Move x y) (Up, _) = ((Up, (x, y)), Nothing)                                    -- if takes 'Move', point, and 'Up' pen's mode, then doesn't draw line (just move pen to another) 
cmd (Move x1 y1) (Down, (x2, y2)) = ((Down, (x1, y1)), Just ((x2, y2), (x1, y1)))   -- if takes 'Move', point, and 'Down' pen's mode, then draw line old point to new point.

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--
prog :: Prog -> State -> (State, [Line])                                            -- prog takes Prog which is list of Cmd and State which is mode and point. 
prog [] s = (s, [])                                                                 -- if input cmd list (Prog) is empty, then nothing will be happened because there is no 'Cmd'
prog p s = linelist p (s, [])                                                       -- if dinput is not empty, then call linelist to append new line in the existed list of lines.


linelist :: Prog -> (State, [Line]) -> (State, [Line])                              -- linelist is for appending new line into existed line list
linelist [] n = n                                                                   -- if existed list was empty, then create list of lines with input value
linelist (x:xs) (s, list) = case cmd x s of
    (st, Nothing) -> linelist xs (st, list)
    (st, Just line) ->  linelist xs (st, list ++ [line])


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = pepe_outline ++ pepe_mouth ++ pepe_eyes

pepe_outline :: Prog                                                            -- pepe the frog, below coordinates are fliped rendering :(
pepe_outline = [Pen Up, Move 38 13, Pen Down,
    Move 41 13,
    Move 43 13,
    Move 45 14,
    Move 48 13,
    Move 50 13,
    Move 52 14,
    Move 53 15,
    Move 53 16,
    Move 55 17,
    Move 56 18,
    Move 57 19,
    Move 57 21,
    Move 56 22,
    Move 55 23,
    Move 54 24,
    Move 54 25,
    Move 55 26,
    Move 56 28,
    Move 56 29,
    Move 55 29,
    Move 55 31,
    Move 52 31,
    Move 51 33,
    Move 48 34,
    Move 43 34,
    Move 39 33,
    Move 35 32,
    Move 32 31,
    Move 31 29,
    Move 31 26,
    Move 32 23,
    Move 33 20,
    Move 34 18,
    Move 36 18,
    Move 38 13]

pepe_mouth :: Prog
pepe_mouth = [Pen Up, Move 55 27, Pen Down,
    Move 54 28,
    Move 52 28,
    Move 49 28,
    Move 47 28,
    Move 44 28,
    Move 42 27,
    Move 40 27,
    Move 38 27,
    Move 38 29,
    Move 39 30,
    Move 40 30,
    Move 42 30,
    Move 43 31,
    Move 45 31,
    Move 47 31,
    Move 50 31,
    Move 51 31,
    Move 53 31, Pen Up, Move 40 28, Pen Down,
    Move 41 29,
    Move 43 29,
    Move 44 29,
    Move 46 29,
    Move 48 29,
    Move 49 29,
    Move 51 30,
    Move 53 29,
    Move 54 29,
    Move 55 29, Pen Up, Move 37 30, Pen Down,
    Move 37 31,
    Move 38 31,
    Move 39 31 ]

pepe_eyes :: Prog
pepe_eyes = [Pen Up, Move 39 17, Pen Down,
    Move 40 16,
    Move 41 15,
    Move 43 15,
    Move 45 16,
    Move 46 16,
    Move 47 16,
    Move 50 16,
    Move 52 16,
    Move 53 16, Pen Up, Move 45 14, Pen Down,
    Move 46 15,
    Move 46 16,
    Move 46 16,
    Move 47 17,
    Move 48 18,
    Move 48 19,
    Move 49 20,
    Move 49 21,
    Move 48 23,
    Move 46 23,
    Move 45 23,
    Move 43 23,
    Move 42 23,
    Move 41 22, Pen Up, Move 48 19, Pen Down,
    Move 47 18,
    Move 45 18,
    Move 44 18,
    Move 42 18,
    Move 41 19,
    Move 39 20,
    Move 38 20,
    Move 39 21,
    Move 40 21,
    Move 41 21,
    Move 43 22,
    Move 44 22,
    Move 45 22,
    Move 47 22, Pen Up, Move 47 17, Pen Down,
    Move 46 17,
    Move 44 17,
    Move 43 17,
    Move 42 17,
    Move 41 17,
    Move 40 18,
    Move 39 18, Pen Up, Move 40 21, Pen Down,
    Move 41 20,
    Move 43 20,
    Move 44 20,
    Move 45 19,
    Move 47 19,
    Move 48 19,
    Move 49 19, Pen Up, Move 43 22, Pen Down,
    Move 43 21,
    Move 44 20,
    Move 45 20,
    Move 46 20,
    Move 47 20,
    Move 48 21,
    Move 48 22, Pen Up, Move 47 22, Pen Down,
    Move 49 22, Pen Up, Move 48 18, Pen Down,
    Move 49 18,
    Move 50 17,
    Move 52 17,
    Move 53 17,
    Move 54 18,
    Move 55 18,
    Move 56 18, Pen Up, Move 48 19, Pen Down,
    Move 49 19,
    Move 50 18,
    Move 52 18,
    Move 53 18,
    Move 54 19,
    Move 55 19,
    Move 56 19,
    Move 57 19, Pen Up, Move 49 20, Pen Down,
    Move 50 20,
    Move 51 20,
    Move 52 20,
    Move 53 20,
    Move 55 20,
    Move 57 21, Pen Up, Move 51 22, Pen Down,
    Move 51 21,
    Move 52 20,
    Move 53 20,
    Move 54 20,
    Move 55 21,
    Move 55 22, Pen Up, Move 49 22, Pen Down,
    Move 56 22, Pen Up, Move 50 23, Pen Down,
    Move 55 23, Pen Up, Move 43 25, Pen Down,
    Move 45 24, Pen Up, Move 49 24, Pen Down,
    Move 50 25,
    Move 53 25] 
