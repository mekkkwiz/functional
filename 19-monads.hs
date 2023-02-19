-- prove that the three monad laws hold for the Either monad
-- hint: prove by cases (Left vs Right)
-- instance Monad (Either e) where
--     	Right m >>= k = k m
--     	Left e  >>= _ = Left e

-- Left identity:
-- return x >>= f
-- = Right x >>= f
-- = f x

-- Thus, return x >>= f is equivalent to f x, and the left identity law holds for the Either monad.

-- Right identity:
-- m >>= return
-- = Right a >>= return
-- = return a
-- = Right a
-- = m

-- m >>= return
-- = Left e >>= return
-- = Left e
-- = m

-- Thus, m >>= return is equivalent to m, and the right identity law holds for the Either monad.

-- Associativity:
-- (m >>= f) >>= g
-- = (Right a >>= f) >>= g
-- = (f a) >>= g
-- = g (f a)

-- (m >>= f) >>= g
-- = (Left e >>= f) >>= g
-- = Left e >>= g
-- = Left e

-- m >>= (\x -> f x >>= g)
-- = Right a >>= (\x -> f x >>= g)
-- = (\x -> f x >>= g) a
-- = f a >>= g

-- m >>= (\x -> f x >>= g)
-- = Left e >>= (\x -> f x >>= g)
-- = Left e

-- Thus, (m >>= f) >>= g is equivalent to m >>= (\x -> f x >>= g), and the associativity law holds for the Either monad.

---------------------------------------------------------------------------------------------------------------------------

-- prove that the three monad laws hold for the list monad
-- instance Monad []  where
-- 	xs >>= f = [y | x <- xs, y <- f x]

-- Left identity:
-- return x >>= f
-- = [x] >>= f
-- = [y | x' <- [x], y <- f x']
-- = [y | y <- f x]
-- = f x

-- Thus, return x >>= f is equivalent to f x, and the left identity law holds for the List monad.

-- Right identity:
-- m >>= return
-- = [y | x <- m, y <- [x]]
-- = m

-- Thus, m >>= return is equivalent to m, and the right identity law holds for the List monad.

-- Associativity:
-- (m >>= f) >>= g
-- = [z | y <- [y' | x <- m, y' <- f x], z <- g y]
-- = [z | x <- m, y <- f x, z <- g y]
-- = [z | x <- m, z' <- [z | y <- f x, z' <- g y]]
-- = [z | x <- m, z' <- f x >>= g]
-- = m >>= (\x -> f x >>= g)

-- Thus, (m >>= f) >>= g is equivalent to m >>= (\x -> f x >>= g), and the associativity law holds for the List monad.

---------------------------------------------------------------------------------------------------------------------------

-- prove that the three monad laws hold for the arrow monad
-- hint: unlock each side of the equality with a value of type r, and check that both sides are indeed equal
-- instance Monad ((->) r) where
-- 	f >>= k = \ r -> k (f r) r

-- Left identity:
-- return x >>= f
-- = (\r -> x) >>= f
-- = \r -> f ((\r -> x) r) r
-- = \r -> f x r

-- On the other hand, f x can be written as a function that takes an argument r:

-- f x r
-- = (\r' -> f x) r

-- Thus, we have shown that return x >>= f is equivalent to f x, and the left identity law holds for the Arrow monad.

-- Right identity:
-- m >>= return
-- = \r -> return (m r) r
-- = \r -> (\r' -> m r') r
-- = m

-- Thus, m >>= return is equivalent to m, and the right identity law holds for the Arrow monad.

-- Associativity:
-- (m >>= f) >>= g
-- = \r -> (m >>= f) r >>= g r
-- = \r -> (f (m r) >>= g) r
-- = \r -> (\r' -> g (f (m r) r') r)
-- = \r -> (\r' -> g (f (m r) r') r') r
-- = \r -> (\r' -> g (f (m r) r') r') (r)
-- = \r -> m r >>= (\x -> f x >>= g) r
-- = m >>= (\x -> f x >>= g)

-- Thus, (m >>= f) >>= g is equivalent to m >>= (\x -> f x >>= g), and the associativity law holds for the Arrow monad.

---------------------------------------------------------------------------------------------------------------------------
-- prove that the three monad laws hold for the pair monad
-- instance Monoid a => Monad ((,) a) where
-- 	(u, a) >>= k = let (v, b) = k a in
-- 		(u <> v, b)

-- Left identity:
-- return x >>= f
-- = (mempty, x) >>= f
-- = let (v, b) = f x in (mempty <> v, b)
-- = f x

-- On the other hand, f x is already equivalent to f x.
-- Thus, we have shown that return x >>= f is equivalent to f x, and the left identity law holds for the Pair monad.

-- Right identity:
-- m >>= return
-- = (u, a) >>= return
-- = let (v, b) = return a in (u <> v, b)
-- = let (v, b) = (mempty, a) in (u <> v, b)
-- = (u <> mempty, a)
-- = (u, a)
-- = m

-- Thus, m >>= return is equivalent to m, and the right identity law holds for the Pair monad.

-- Associativity:
-- (m >>= f) >>= g
-- = let (v, b) = (u, a) >>= f in (v <> w, c)
-- = let (v, b) = let (v', b') = f a in (u <> v', b') in (v <> w, c)
-- = let (v', b') = f a in let (v, b) = (u <> v', b') in (v <> w, c)
-- = let (v', b') = f a in (u <> v' <> w, c)
-- = (u, a) >>= (\x -> let (v, b) = f x in (v <> w, b))
-- = m >>= (\x -> f x >>= g)

-- Thus, (m >>= f) >>= g is equivalent to m >>= (\x -> f x >>= g), and the associativity law holds for the Pair monad.