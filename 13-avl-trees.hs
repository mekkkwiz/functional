data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

-- Q: how should we define equality for binary trees?
instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    Node x left right == Node x' left' right' = (x == x') && (left == left') && (right == right')
    _ == _ = False

-- Q: when are two binary trees equal?
-- A: Two binary trees are considered equal when they have the same structure and the same values at each node.


-- Q: how should we define equality for lists?
-- A: comparing list elements one by one. If all the elements are the same then the lists are considered equal, otherwise they are not.


-- Q: when are two lists equal?
-- A: two lists are considered equal when they have the same length and the same elements in the same order.



-- Q: how should we define equality for pairs?
-- A: compares the first element of the first pair with the first element of the second pair,
-- and the second element of the first pair with the second element of the second pair.
-- If any pair of corresponding elements are not equal, it returns False and vice versa it returns True.


-- Q: when are two pairs equal?
-- A: two pairs are considered equal when their first and second elements are equal.


-- Q: how should we define equality for Maybe?
-- Q: Values are both Just values, but the value inside is different, i.e. 1 and 2 respectively. Therefore, these two Maybe values are not considered equal.


-- Q: values are considered equal when they are both Nothing are both Just values and the values inside are equal.
-- A: Two Maybe values are considered equal when they are both Nothing, or if they are both Just values and the values inside are equal.