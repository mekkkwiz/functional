-- show that (,) is a bifunctor by implementing
-- pairBimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
pairBimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
pairBimap f g (a, b) = (f a, g b)


-- prove that these bifunctor laws hold:
-- pairBimap id id = id
-- pairBimap id id (a, b) = (id a, id b) = (a, b) = id (a, b)
-- Therefore, pairBimap id id = id holds.

-- The second bifunctor law is: pairBimap (f . g) (h . i) = pairBimap f h . pairBimap g i
-- pairBimap (f . g) (h . i) (a, b)
-- = ((f . g) a, (h . i) b)
-- = (f (g a), h (i b))
-- = pairBimap f h (g a, i b)
-- = (pairBimap f h . pairBimap g i) (a, b)
-- Therefore, pairBimap (f . g) (h . i) = pairBimap f h . pairBimap g i holds.


-- write the following natural transformations
-- Maybe → list
-- maybeToList :: Maybe a -> [a]
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]


-- true or false, and why: for any f :: a -> b,
-- fmap f . maybeToList = maybeToList . fmap f
-- True. The reason why this is true is because both fmap f . maybeToList and maybeToList . fmap f have the same type signature of Maybe a -> [b].


-- list → Maybe
-- listToMaybe :: [a] -> Maybe a
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


-- true or false, and why: for any f :: a -> b,
-- fmap f . listToMaybe = listToMaybe . fmap f
-- True. The reason same as before.