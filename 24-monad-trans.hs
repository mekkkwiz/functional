-- implement a monad transformer for Identity monad:
    -- IdentityT m a, wrapping m a\
import Control.Monad.Trans
import Control.Applicative
import Data.Char


newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT x) = IdentityT (fmap f x)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  IdentityT f <*> IdentityT x = IdentityT (f <*> x)

instance (Monad m) => Monad (IdentityT m) where
  IdentityT x >>= f = IdentityT (x >>= runIdentityT . f)

-- implement a monad transformer for Either monad:
    -- EitherT a m b, wrapping m (Either a b)

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

instance (Functor m) => Functor (EitherT a m) where
  fmap f (EitherT x) = EitherT (fmap (fmap f) x)

instance (Applicative m) => Applicative (EitherT a m) where
  pure x = EitherT (pure (Right x))
  EitherT f <*> EitherT x = EitherT (liftA2 (<*>) f x)

instance (Monad m) => Monad (EitherT a m) where
  EitherT m >>= f = EitherT $ do
    res <- m
    case res of
      Left err -> return (Left err)
      Right val -> runEitherT (f val)

-- implement a monad transformer for the arrow monad:
    -- ContT r m a, wrapping m (r -> a)

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance (Functor m) => Functor (ContT r m) where
  fmap f (ContT x) = ContT $ \k -> x (k . f)

instance (Monad m) => Applicative (ContT r m) where
  pure x = ContT $ \k -> k x
  ContT f <*> ContT x = ContT $ \k -> f $ \f' -> x (k . f')

instance (Monad m) => Monad (ContT r m) where
  ContT x >>= f = ContT $ \k -> x $ \a -> runContT (f a) k

-- modify the signup page example to use EitherT transformer
-- be sure to give helpful error messages

readEmail :: IO (Either String String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Right str
    else return $ Left "Invalid email!"

readPassword :: IO (Either String String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || not (any isUpper str)
      || not (any isLower str)
    then return $ Left "Invalid Password"
    else return $ Right str

signup :: EitherT String IO ()
signup = do
  email <- EitherT readEmail
  password <- EitherT readPassword
  password2 <- EitherT readPassword
  if password == password2
    then return ()
    else EitherT $ return $ Left "Password mismatch"

main :: IO ()
main = do
  res <- runEitherT signup
  case res of
    Left err -> putStrLn $ "Signup failed: " ++ err
    Right _ -> putStrLn "Signup success"