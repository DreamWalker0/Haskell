--Author:Jorge Guzman Nader
--      :Christa Wright
--Info: This program inplements several functions for manipulating trees

module HW1 where


-- | Integer-labeled binary trees.
data Tree
   = Node Int Tree Tree   -- ^ Internal nodes
   | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf a) = a --Base Case
leftmost (Node _ left _) = leftmost left -- Recursive call to get left leaf

-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree -> Int
rightmost (Leaf b) = b -- Base Case
rightmost (Node _ _ right) = rightmost right -- Recursive call to get right leaf


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
maxInt :: Tree -> Int
maxInt (Leaf a) = a -- Base Case
maxInt (Node a left right) = max a (max (maxInt left) (maxInt right))
-- Recursive call uses max :: Ord a => a -> a -> a

-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt :: Tree -> Int
minInt (Leaf a) = a -- Base case
minInt (Node a left right) = min a (min(minInt left) (minInt right))
-- Recursive call that uses min :: Ord a => a -> a -> a


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
sumInts:: Tree -> Int
sumInts (Leaf a) = a -- Base case
sumInts (Node a left right) = a + (sumInts left) + (sumInts right) -- Adds all nodes in the tree in the form of (Parent + right + left)

-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--
preorder:: Tree -> [Int]
preorder (Leaf x) = [x] --Base case
preorder (Node x left right) = [x] ++ (preorder left) ++ (preorder right)
-- The recursive call concatenates the elements of each node to put them in a list

-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--
inorder:: Tree -> [Int]
inorder (Leaf a) = [a] --Base case
inorder (Node a left right) = (inorder left) ++ (a : inorder right)
-- The recursive call, calls first the left node then it adds "a" as the first element of the --- list for the right node


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
isBST:: Tree -> Bool
isBST (Leaf a) = True
isBST (Node a left right)=
  if ((maxInt left) < a) -- If the left subtrees has nodes less than the parent
  && ((minInt right) > a) -- And the right subtree is bigger than the Parent
  then True else False   -- Then it is a binary tree, else is not

-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
inBST:: Int -> Tree -> Bool
inBST x (Leaf root) = -- Where x is the number we are looking for
  if x == root then True else False -- Base case
  -- If there is a number x that is equal to the root then it return True
inBST x (Node root left right) =
  if x == root then True
  else (inBST x left) || (inBST x right)
