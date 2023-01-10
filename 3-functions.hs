-- write function zipper' that is a curried version of zipper
zipper' :: [a]->[b]->[(a,b)]
zipper' [] _ = []
zipper' _ [] = []
zipper' (x:xs) (y:ys) = (x,y) : zipper' xs ys

-- Q: what's the type of zipper'?
-- A: [a]->[b]->[(a,b)]

-- Q: what's the type of zipper' []?
-- A: [b]->[(a,b)]

-- Q: what does zipper' [] do?
-- A: it takes [b] for arguments and returns only the empty list

-- Q: is there a simpler way to express the same thing?
-- A: yes, using the constant function that returns only empty list

-- write function fac that computes the factorial of a given nonnegative number
-- type Int can overflow
-- type Integer is unbounded
-- fac :: Int -> Int
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

-- Q:  what's the type of fac?
-- A:  Int -> Int

-- Q:  is your implementation of fac safe?
-- A:  no

-- Q:  if not, when is it not safe, and what should you do to make it safer?
-- A:  it will crash if you give it a negative number or type other than Int
--     I think we should have if-condition to check input type if
--     it's not nonnegative number stop the program



-- f = (t1, t2, [t1], [t2]) -> [(t1, t2)]
-- Q: what should be the type of the result of fully currying f?
-- A: t1 -> t2 -> [t1] -> [t2] -> [(t1, t2)]

-- Q: what do you think f should do?
-- A: it should take 4 arguments and return a list of tuples
