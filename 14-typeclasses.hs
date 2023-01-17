-- Q: define instances of IfValue for as many types as you can
class IfValue a where
    boolVal :: a -> Bool

instance IfValue Int
    where
    boolVal 0 = False
    boolVal _ = True
instance IfValue Char
    where
    boolVal '\0' = False
    boolVal _ = True
instance IfValue Bool
    where
    boolVal = id
instance IfValue [a]
    where
    boolVal [] = False
    boolVal _ = True
instance (IfValue a) => IfValue (Maybe a)
    where
    boolVal (Just x) = boolVal x
    boolVal Nothing = False

-- Q: define map for Maybe type
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing = Nothing

-- Q: define map for pairs, if you can
mapPair :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
mapPair f g (x, y) = (f x, g y)


