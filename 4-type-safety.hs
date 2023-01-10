join :: ([a], [a]) -> [a]
join (f, []) = f
join ([], b) = b
join (f:fs, b) = f : join (fs, b)

-- recall your implementation of list reverse:
rev :: ([a]) -> [a]
rev ([]) = []
rev (x:xs) = join (rev(xs), [x])

-- Q: how long does join take?
-- A: O(n) where n is the length of the first list

-- Q: how long does your rev take to compute the reverse of a list ? (briefly explain why)
-- A: O(n^2) because the join function is called n times and each time it takes O(n) time to run

-- Q: are you satisfied with the running time if not, what would you like to do to improve the efficiency?
-- A: No, I would like to make a new function that takes O(n) time to run
-- by make empty list and add the last element of the list to the front of the empty list

-- write a function for Fibonacci numbers:
-- please take care of improper inputs
fib :: Integer -> Integer
fib n
    | n < 0     = error "negative input"
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib (n-1) + fib (n-2)

-- Q: what's the type of your fib?
-- A: Integer -> Integer

-- Q: how long does your fib take to compute fib n ? (briefly explain why)
-- A: O(2^n) because the fib function is called 2^n times from fib (n-1) and fib (n-2)

-- Q: are you satisfied with the running time of fib if not, what would you like to do to improve the efficiency?
-- A: No, I would like to use this algorithm

ffib :: Integer -> Integer
ffib = fastFib 1 1

fastFib :: Integer -> Integer -> Integer -> Integer
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib a b 3 = a + b
fastFib a b c = fastFib (a + b) a (c - 1)












