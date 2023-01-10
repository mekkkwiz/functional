-- consider partition function:
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs
-- Q: what's the type of partition?
-- A: partition :: (a -> Bool) -> [a] -> ([a], [a])

-- Q: what does partition do?
-- A: partition is a function that takes a predicate and a list as input
-- and returns a tuple of two lists. The first list contains all elements
-- of the input list that satisfy the predicate, the second list contains
-- all elements of the input list that do not satisfy the predicate.

-- Q: rewrite filter using partition
filter' p = fst . partition p

-- Q: rewrite quicksort without using list comprehension?
quicksort [] = []
quicksort (x:xs) = quicksort l ++ [x] ++ quicksort r
    where (l, r) = partition (<x) xs

-- look up type Ordering
    -- Q: what is it for?
    -- A: Ordering is a type that can be either LT, EQ or GT. It is used to
    -- compare two values of the same type. The function compare :: Ord a => a -> a -> Ordering
    -- compares two values of type a and returns LT if the first value is less than the second,
    -- EQ if the values are equal and GT if the first value is greater than the second value.

    -- Q: how many constructors are there?
    -- A: 3
    -- data Ordering = LT | EQ | GT

    -- Q: how many ways can we pattern-match an Ordering value How?
    -- A: 3 ways
    -- case x of
    --     LT -> ...
    --     EQ -> ...
    --     GT -> ...