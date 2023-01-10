-- Q: write function elem that determines whether a given element is a member of a list
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' input list = foldl (\acc x -> x == input && acc) True list

-- Q: what's the type of elem?
-- A: (Foldable t, Eq a) => a -> t a -> Bool

-- Q: can you use fold?
-- A: yes

-- Q: analyze efficiency of both versions of map that use fold?
map_rf :: (a -> b) -> [a] -> [b]
map_rf f list = foldr (\x xs -> f x : xs) [] list

map_lf :: (a -> b) -> [a] -> [b]
map_lf f list = reverse . foldl (\acc x -> f x : acc) [] $ list

-- A: For the first version that using foldr, which is right-associative and builds up the result by
-- adding elements to the front of the list.
-- For the second version that using foldl, which is left-associative and builds up the result by
-- adding elements to the end of the list. And need to reverse the result that make this version of map
-- less efficient than the first version if result list is long

-- Q: rewrite partition using fold
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition pred list = foldr aux ([],[]) list
    where aux x (t_list, false_list) = if pred x then (x:t_list, false_list) else (t_list, x:false_list)


-- Q: define foldl in terms of foldr
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' func acc list = foldr (flip func) (id acc) list

-- Q: can you do the other direction?
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' func acc list = foldl (flip func) (id acc) list