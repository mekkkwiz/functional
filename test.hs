data Tree a = Empty | Node (Tree a) a (Tree a) | Leaf a deriving (Show)


-- Q: define map for binary trees
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f tree = case tree of
    Empty -> Empty
    Node l x r -> Node (mapTree f l) (f x) (mapTree f r)
    Leaf x -> Leaf (f x)

-- Q: what's the mapTree type?
-- mapTree :: (a -> b) -> Tree a -> Tree b



-- Q: define fold for binary trees
foldPreTree :: (a -> b -> b) -> b -> Tree a -> b
foldPreTree f acc tree = case tree of
    Empty -> acc
    Node l x r -> f x (foldPreTree f (foldPreTree f acc l) r)

foldInTree :: (a -> b -> b) -> b -> Tree a -> b
foldInTree f acc tree = case tree of
    Empty -> acc
    Node l x r -> foldInTree f (f x (foldInTree f acc r)) l

foldPostTree :: (a -> b -> b) -> b -> Tree a -> b
foldPostTree f acc tree = case tree of
    Empty -> acc
    Node l x r -> foldPostTree f (foldPostTree f (f x acc) r) l

-- Q: what's the fold type?
-- foldPreTree  :: (a -> b -> b) -> b -> Tree a -> b
-- foldInTree   :: (a -> b -> b) -> b -> Tree a -> b
-- foldPostTree :: (a -> b -> b) -> b -> Tree a -> b

-- Q: write a function height that returns the heights of a given binary tree
heightOf :: Tree a -> Integer
heightOf tree = case tree of
    Empty -> 0
    Node l _ r -> 1 + max (heightOf l) (heightOf r)

-- Q: write a function isBST that takes a binary tree, and determine if it is a binary search tree
isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node l x r) = isBST l && isBST r && allSmaller x l && allBigger x r
  where
    allSmaller _ Empty = True
    allSmaller x (Node l y r) = y < x && allSmaller x l && allSmaller x r
    allBigger _ Empty = True
    allBigger x (Node l y r) = y > x && allBigger x l && allBigger x r

testIsBST :: IO ()
testIsBST = do
  let t1 = Node (Node (1) 2 (3)) 4 (Node (5) 6 (7))
      t2 = Node (Node (7) 6 (5)) 4 (Node (3) 2 (1))
      t3 = Node (Node (1) 2 (3)) 2 (Node (5) 6 (7))
      t4 = Node (Node (7) 6 (5)) 6 (Node (3) 2 (1))
      t5 = 1
  print $ isBST t1 -- should print True
  print $ isBST t2 -- should print False
  print $ isBST t3 -- should print False
  print $ isBST t4 -- should print False
  print $ isBST t5 -- should print True