-- Team members
-- Junhyeok Jeong, jeongju@oregonstate.edu
-- Youngjoo Lee, leey3@oregonstate.edu
module HW2 where

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--

-- input value is a list type, and output is Tree data
encodeList :: [x] -> Tree x
-- If the input is empty, then print End
encodeList [] = End
-- If the input is not empty, then print h (head) part of node 
-- and then recall function for rest of the input list
encodeList (h:t) = Node h End (encodeList t)


-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
-- the input form function Tree and the output is Tree data
mapTree :: (x -> y) -> Tree x -> Tree y
-- if the input Tree is End, then print End
mapTree f End = End
-- if the input Tree has Node, then apply function on Node x first
-- and call recursively for Node x's left and right
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
-- the input is Path data and the output is Maybe a value in the inputted tree or Nothing
valueAt :: Path -> Tree x -> Maybe x
-- if the input Tree is End, then Nothing
valueAt x End = Nothing
-- if the input path is root, then print Just root
valueAt [] (Node x left right) = Just x
-- if the input path is from left, then call recursively for left leaf of inputted node
valueAt (L:path) (Node x left right) = valueAt path left
-- if the input path is from right, then call recursively for right leaf of inputted node
valueAt (R:path) (Node x left right) = valueAt path right


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
-- The constraint Eq a => means that the function will work for all trees that contain values that 
-- can be checked for equality (like integers, characters, and booleans).
-- The output form is Just Path or Nothing
pathTo :: Eq a => a -> Tree a -> Maybe Path
-- If the input Tree is End, then print Nothing
pathTo val End = Nothing
-- If the input Tree is valid, then
-- 1. If search value (val) is the same as root, then print Just []
-- 2. otherwise, use case-expression to check left and right side of the Tree node
pathTo val (Node x left right)
  | val == x = Just []
  | otherwise = case (pathTo val left, pathTo val right) of
      (Just path, y) -> Just (L:path)
      (y, Just path) -> Just (R:path)
      y -> Nothing