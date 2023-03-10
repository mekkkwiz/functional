-- implement functions that have the following types:
import Data.Void

-- (a -> b -> c) -> (a -> b) -> a -> c
f1 :: (a -> b -> c) -> (a -> b) -> a -> c
f1 f g x = f x (g x)

-- (a, b) -> Either a b
f2 :: (a, b) -> Either a b
f2 (x, y) = if True then Left x else Right y


-- (a -> b, a) -> b
f3 :: (a -> b, a) -> b
f3 (f, x) = f x


-- Either (a -> Void) b -> a -> b
f4 :: Either (a -> Void) b -> a -> b
f4 (Left f) x = absurd (f x)
f4 (Right y) _ = y


-- write a proof that corresponds to each of the written functions
-- f1
-- (g x) => (type b) then apply to f (f = (a -> b -> c))
-- like this `f x (g x) => f (type a) (type b)` now f can produce type c

-- f2
-- this condition in the if expression is always True then this function always returns a value of type Either a b
-- If we pass (x, y) as an argument it returns Left x which is a value of type Either a b.

-- f3
-- f = (a -> b)
-- (f x) => (type b)

-- f4
-- Let e => (type Either (a -> Void) b) and let x => (type a) Then we have two cases to consider:

-- If e = Left f
-- f = (a -> Void)
-- then there is no value of type Void so we can use the absurd function to convert f x to any other type.
-- (Left f) x = absurd (f x) make this function produce a value of type b

-- or If e is Right y
-- then we can just return y because y => `type b`
