-- | This module is just to introduce the doctest tool.
module Doctest where


-- | This function should add two numbers together.
--   Here are some tests to see if it works:
--
--   >>> add 2 2
--   4
--
--   >>> add 2 5
--   6
--   >>> add 3 3
--   3
add :: Int -> Int -> Int
add x y = x + y