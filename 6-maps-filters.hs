-- Q: write function filter_concat that takes a predicate and a list of lists, and returns the result of concatenating lists that pass the predicate
filter_concat :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat _ [] = []
filter_concat p (x:xs)
    | p x = x ++ filter_concat p xs
    | otherwise = filter_concat p xs

-- Q: what's the type of filter_concat?
-- A: ([a] -> Bool) -> [[a]] -> [a]

-- Q: can you avoid recursion in your definition?(feel free to use functions from the Prelude)
-- A: Yes, I can use filter and concat functions
filter_concat' :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat' f xs = concat (filter f xs)

-- Q: write function take_while that keeps taking elements from the beginning of the given list, as long as these elements pass the predicate
take_while :: (a -> Bool) -> [a] -> [a]
take_while _ [] = []
take_while p (x:xs)
    | p x = x : take_while p xs
    | otherwise = []

-- Q: what's the type of take_while?
-- A: take_while :: (a -> Bool) -> [a] -> [a]

-- Q: rewrite (\l -> length l < 3) without writing lambdas (\)
-- A: \l -> length l < 3 = (<3) . length