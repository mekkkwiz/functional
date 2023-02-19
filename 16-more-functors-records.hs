type CourseID = Int
type Capacity = Int
type StudentID = Int

data CourseInfo' = Course'
  { cid :: CourseID
  , cap :: Capacity
  , roster :: [StudentID]
  } deriving (Show)


enroll'' :: CourseInfo' -> StudentID -> Either String CourseInfo'
enroll'' c sid
  | sid `elem` rs =
      Left "student already registered"
  | length rs >= seats =
      Left "course full"
  | otherwise = Right $
      Course' (cid c) seats (sid:rs)
  where seats = cap c
        rs = roster c

register'' :: [CourseInfo'] -> CourseID -> StudentID -> Either String [CourseInfo']
register'' [] _ _ = Left "no such course"
register'' (c : cs) cid' sid
  | cid c == cid' = 
      case enroll'' c sid of
        Left msg -> Left msg
        Right c' -> Right (c' : cs)
  | otherwise = register'' cs cid' sid


courses = [Course' 1 5 [], Course' 2 4 []]


-- don't know


-- recall that for a functor f:
-- fmap :: (a -> b) -> f a -> f b
-- and that Maybe and list are functors
-- Q: define function maybeAp ::
-- A:
maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing
maybeAp _ Nothing = Nothing
maybeAp (Just f) (Just a) = Just (f a)

-- Q: define function initMaybe :: a -> Maybe a
-- A:
initMaybe :: a -> Maybe a
initMaybe a = Just a

-- Q: define function listAp ::
-- A:
listAp :: [a -> b] -> [a] -> [b]
listAp fs as = [f a | f <- fs, a <- as]

-- Q: define function initList :: a -> [a]
-- A:
initList :: a -> [a]
initList a = [a]

-- Q: explain what fmap (*3) (+100) is
-- A: fmap (*3) (+100) is a function that takes an integer and returns
--    the result of applying (+100) to it and then applying (*3) to the result



