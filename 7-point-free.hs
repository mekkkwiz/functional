-- Q: write this function in point-free style:
    -- contains1 = \x l -> any (x<) l
    -- hint: any :: (a -> Bool) -> [a] -> Bool
-- A:
-- \x l -> any (x<) l
-- \x -> any (x<)
-- any . (<)

-- Q: write this function in point-free style:
    -- contains2 = \l x -> any (x<) l
    -- hint: use flip function
-- A:
-- \l x -> any (x<) l
-- \l -> any . (<) l
-- \l -> any . flip (<) l
-- any . flip (<)

-- Q: write function len_comp that uses list comprehension
-- to compute the length of the given list by using sum function
len_comp :: [a] -> Int
len_comp = \l -> sum [1 | _ <- l]

-- Q: rewrite [(x,y) | x <- [2,3,5], y <- [1,2,4],even $ x+y] without using list comprehension
f = filter (\(x,y) -> (even (x+y))) $ concatMap (\x -> map (\y -> (x,y)) [1,2,4]) [2,3,5]
