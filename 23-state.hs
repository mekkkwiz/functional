import Control.Monad.State

type Queue a = ([a], [a], Int)

size :: State (Queue a) Int
size = do
  (_, _, s) <- get
  return s

isEmpty :: State (Queue a) Bool
isEmpty = do
  (f, b, _) <- get
  return $ null f && null b

enqueue :: a -> State (Queue a) ()
enqueue x = do
  (f, b, s) <- get
  put (f, x:b, s+1)

dequeue :: State (Queue a) a
dequeue = do
  (f, b, s) <- get
  case f of
    [] -> case reverse b of
            [] -> error "Queue is empty"
            (x:xs) -> do
              put (xs, [], s-1)
              return x
    (x:xs) -> do
      put (xs, b, s-1)
      return x

mkQueue :: [a] -> State (Queue a) ()
mkQueue xs = put (xs, [], length xs)

empty :: State (Queue a) ()
empty = put ([], [], 0)


queueManip = do
  mkQueue [1..5]
  s1 <- size
  dequeue
  enqueue 6
  enqueue 7
  dequeue
  dequeue
  dequeue
  dequeue
  s2 <- dequeue
  return (s1,s2)