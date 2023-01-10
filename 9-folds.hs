-- Q: rewrite list reverse using left fold
reverse_lf list = foldl (flip (:)) [] list

-- Q: rewrite list reverse using right fold
reverse_rf list = foldr (\x xs -> xs ++ [x]) [] list

-- Q: which version is more efficient, and why?
-- A: left fold is more efficient, because it doesn't need to traverse the list twice

-- Q: rewrite map using fold
map' f list = foldr (\x xs -> f x : xs) [] list

-- Q: rewrite filter using fold
filter' f list = foldr (\x xs -> if f x then x : xs else xs) [] list

