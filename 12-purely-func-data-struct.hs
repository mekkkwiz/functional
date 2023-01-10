data Tree a = Empty | Node (Tree a) a (Tree a) | Leaf a
    deriving (Show)

-- Q: define map for binary trees
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f tree = case tree of
    Empty -> Empty
    Leaf x -> Leaf (f x)
    Node l x r -> Node (mapTree f l) (f x) (mapTree f r)

-- Q: what's the mapTree type?
-- mapTree :: (a -> b) -> Tree a -> Tree b



-- Q: define fold for binary trees
foldPreTree :: (a -> b -> b) -> b -> Tree a -> b
foldPreTree _ acc Empty = acc
foldPreTree f acc (Leaf x) = f x acc
foldPreTree f acc (Node l x r) = f x (foldPreTree f (foldPreTree f acc l) r)

foldInTree :: (a -> b -> b) -> b -> Tree a -> b
foldInTree _ z Empty = z
foldInTree f z (Leaf x) = f x z
foldInTree f z (Node l x r) = foldInTree f (f x (foldInTree f z r)) l

foldPostTree :: (a -> b -> b) -> b -> Tree a -> b
foldPostTree _ z Empty = z
foldPostTree f z (Leaf x) = f x z
foldPostTree f z (Node l x r) = foldPostTree f (foldPostTree f (f x z) r) l

-- Q: what's the fold type?
-- foldPreTree  :: (a -> b -> b) -> b -> Tree a -> b
-- foldInTree   :: (a -> b -> b) -> b -> Tree a -> b
-- foldPostTree :: (a -> b -> b) -> b -> Tree a -> b

-- Q: how many different folds can you come up with?
-- A: 3

-- Q: write a function height that returns the heights of a given binary tree
heightOf :: Tree a -> Integer
heightOf tree = case tree of
    Empty -> 0
    Leaf _ -> 1
    Node l _ r -> 1 + max (heightOf l) (heightOf r)

-- Q: write a function isBST that takes a binary tree, and determine if it is a binary search tree
isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Leaf _) = True
isBST (Node l x r) = isBST l && isBST r && isSmaller x l && isBigger x r
    where
        -- isSmaller checks if all the values in the given tree are smaller than x
        isSmaller _ Empty = True
        isSmaller x (Leaf y) = y < x
        isSmaller x (Node l y r) = y < x && isSmaller x l && isSmaller x r
        -- isBigger checks if all the values in the given tree are bigger than x
        isBigger _ Empty = True
        isBigger x (Leaf y) = y > x
        isBigger x (Node l y r) = y > x && isBigger x l && isBigger x r

-- Q: what's the type?
-- isBST :: Ord a => Tree a -> Bool

-- Q: define type NAryTree for n-ary trees, and implement preorder and postorder traversals
data NAryTree a = NLeaf a | NNode a [NAryTree a]
    deriving (Show)

preorder :: NAryTree a -> [a]
preorder (NLeaf x) = [x]
preorder (NNode x children) = x : concatMap preorder children

postorder :: NAryTree a -> [a]
postorder (NLeaf x) = [x]
postorder (NNode x children) = concatMap postorder children ++ [x]