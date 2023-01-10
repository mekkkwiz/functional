-- takes a function and an input list, and returns the list after applying the function to each element
list_map :: (a -> b) -> [a] -> [b]
list_map = list_map_aux []
    where
        list_map_aux :: [b] -> (a -> b) -> [a] -> [b]
        list_map_aux acc _ [] = acc
        list_map_aux acc f (x:xs) = list_map_aux (acc ++ [f x]) f xs

-- Q: what's the type of list_map?
-- A: (a -> b) -> [a] -> [b]

-- Q: can you use tail recursion?
-- A: Yes, I can use tail recursion

-- Q: write three more test cases for list_map
-- A: 1. list_map (\x -> 5**x > 10000) [1..10] = [False,False,False,False,False,True,True,True,True,True]
--    2. list_map (++"!") ["hello", "world"] = ["hello!", "world!"]
--    3. list_map (=='l') "hello world" = [False, False, True, False, False, False, False, False, False, False]

-- Q: create zipper using tail recursion
zip_tr :: [a]->[b]->[(a,b)]
zip_tr = zip_aux []
    where
        zip_aux :: [(a,b)] -> [a] -> [b] -> [(a,b)]
        zip_aux acc [] [] = reverse acc
        zip_aux acc _ [] = reverse acc
        zip_aux acc [] _ = reverse acc
        zip_aux acc (x:xs) (y:ys) = zip_aux ((x,y):acc) xs ys
-- Note: reverse is also tail-recursive so the whole operation is tail-recursive.
-- ref: https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.List.html#reverse

-- Challenge
facs = 1 : zipWith (*) [1..] facs
-- Q: define fibs, the list of Fibonacci numbers, in the same way as facs
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

