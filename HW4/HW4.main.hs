--Authors:
-- Christa Wright
-- Jorge G Nader
-- Info: This program implements a language to draw geometric figures
-- CS 381

module HW3 where

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
cmd :: Cmd -> State -> (State, Maybe Line) --ask last case

cmd ( Pen modus ) (_, ( x , y ))        = ((modus,(x,y)), Nothing)
-- cmd ( Move x  y ) ( modus ,_ )            = ((modus,(x,y)),Nothing)
cmd (Move i j) (Down,(x,y))         = ((Down, (i,j)),Just ((x,y),(i,j)))
cmd (Move i j) (Up, (x,y))            = ((Up, (i,j)), Nothing)



-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line]) -- need to fix the [] when testing
-- prog w x = case w of
--   [] -> (x, []) -- Empty list
--   (v:vs) -> case cmd v x of
--     (state2, Nothing) -> prog vs state2
--     (state2, Just y) -> (\(x, vs) -> (x,y:vs)) (prog vs state2)
prog [] state = (state, [])
prog (x:xs) state = case cmd x state of
     (ys, Just i) -> (\(state,xs) -> (state, i:xs))  (prog xs ys) -- $ f x = f(x)
     (ys, Nothing) -> prog xs ys -- let x = e1 in e2 eval to e1 give result e2


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = penDraw (0,21) ++
  --outline
  [Move 12 27, Move 12 40, Move 24 31, Move 32 36, Move 32 22, Move 40 18]++
  [Move 28 11, Move 28 1, Move 16 8, Move 8 3, Move 8 17, Move 0 21]++

  penDraw(16,8)++
  [Move 20 10, Move 24 8, Move 28 11 ]++
  penDraw(24 ,8)++
  [Move 13 14, Move 20 18, Move 28 14, Move 28 28, Move 24 31]++
  penDraw(32, 22)++
  [Move 28 24]++
  penDraw(20, 27)++
  [Move 8 20, Move 8 17]++
  penDraw(12, 27)++
  [Move 12 22]++
  penDraw(20, 27)++
  [Move 20 18]



          -- [Move 10 15,Move 15 18, Move 20 15, Move 20 10, Move 15 7, Move 10 10 ] ++ --Hexagon
          -- [Move 2 23, Move 0 20, Move 3 20]
          -- penDraw (7, 23) ++
          -- [Move 4 23, Move 4 22, Move 7 22, Move 7 20, Move 4 20] ++
          -- penDraw (8, 23) ++
          -- [Move 11 23, Move 11 22, Move 8 22, Move 11 22, Move 11 20, Move 8 20] ++
          -- penDraw (15, 22) ++
          -- [Move 15 23, Move 12 23, Move 12 22, Move 15 22, Move 15 20, Move 12 20, Move 12 22] ++
          -- penDraw (16, 23) ++
          -- [Move 16 20]




-- Helper Function for Drawing lines
penDraw :: (Int, Int) -> Prog
penDraw (x, y) = [Pen Up, Move x y, Pen Down]
