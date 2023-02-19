-- give three more examples of monoids
-- the carrier sets must be different from the examples and from each other
-- be sure to reason about monoid laws

-- carrier: set of natural numbers, represented as {1, 2, 3, 4, ...}.
-- operator: addition (+)
-- identity: 0
-- 0 serves as an identity for all elements in N, making this a valid monoid.
-- x + (y + z) = (x + y) + z
-- x + 0 = x = 0 + x

-- carrier: Matrices
-- operator: Matrix Multiplication
-- identity: Identity Matrix
-- (A * B) * C = A * (B * C)
-- A * I = A = I * A

-- carrier: Boolean
-- operator: logical AND
-- identity: True
-- (A and B) and C = A and (B and C)
-- A and True = A = True and A

-- using newtype, declare monoid instances for Bool, where
-- operator: (&&)
-- operator: (||)

newtype And = And Bool deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup And where
    (And x) <> (And y) = And (x && y)

instance Monoid And where
    mempty = And True

newtype Or = Or Bool deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Or where
    (Or x) <> (Or y) = Or (x || y)

instance Monoid Or where
    mempty = Or False

-- define function maybeBind ::
-- Maybe a -> (a -> Maybe b) -> Maybe b

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x

-- define function listBind ::
-- [a] -> (a -> [b]) -> [b]

listBind :: [a] -> (a -> [b]) -> [b]
listBind x f = concat (map f x)

-- define function eitherBind ::
-- Either r a -> (a -> Either r b) -> Either r b

eitherBind :: Either r a -> (a -> Either r b) -> Either r b
eitherBind (Left x) _ = Left x
eitherBind (Right x) f = f x

-- define function arrowBind ::
-- (r -> a) -> (a -> (r -> b)) -> (r -> b)

arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
arrowBind f g r = g (f r) r

-- define function pairBind ::
-- (r, a) -> (a -> (r, b)) -> (r, b)
-- what do we need to know about r?

pairBind :: (r, a) -> (a -> (r, b)) -> (r, b)
pairBind (x, y) f = f y
-- The first component r of the input tuple is not used in the computation
-- so we don't need to know anything specific about r
