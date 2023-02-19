-- suppose we want to make (,) r an applicative functor
-- that means we need to implement
-- (<*>) ::
--   (r, a -> b) -> (r, a) -> (r, b)
-- r could be any type, for representing labels
-- but now we have two pieces of data containing labels (i.e., (r,a->b) and (r,a))
-- how should we implement (<*>) so that the resulting data contain appropriate label?

(<*>) :: (r, a -> b) -> (r, a) -> (r, b)
(r1, f) <*> (r2, a) = (r1, f a)

-- how should we implement pure so that the initial label makes sense?

pure :: r -> a -> (r, a)
pure r a = (r, a)

-- prove that the four applicative functor laws hold for(->) r applicative functor
-- hint: apply each side of the equality to an argument (of type r), and check that both sides are indeed equal
-- instance Applicative ((->) r) where
-- 	pure = const
-- 	(<*>) f g x = f x (g x)

-- Identity:
-- pure id <*> v
-- = const id <*> v
-- = (\x -> id (v x))
-- = (\x -> v x)
-- = v

-- Composition:
-- pure (.) <*> u <*> v <*> w
-- = const (.) <*> u <*> v <*> w
-- = (\x -> (.) (u x) (v x)) (w x)
-- = (u x) . (v x) (w x)
-- = (u <*> (v <*> w)) x

-- Homomorphism:
-- pure f <*> pure x
-- = const f <*> const x
-- = (\x -> f (const x x))
-- = (\x -> f x)
-- = pure (f x)

-- Interchange:
-- u <*> pure y
-- = (\x -> u x y)
-- = (\x -> ($ y) (u x))
-- = pure ($ y) <*> u
