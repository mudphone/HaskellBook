module TwentySix where

import Control.Monad (liftM)
import Control.Monad.IO.Class --(MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)

-- Exercises: EitherT
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

-- 1
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

-- 2
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x

  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

-- 3
instance Monad m => Monad (EitherT e m) where
  return = pure

  (EitherT me) >>= f =
    EitherT $ do
      v <- me
      case v of
        Left x -> return $ Left x
        Right y -> runEitherT (f y)

-- 4
swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ swapEither <$> me

-- 5
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT me) = do
  v <- me
  case v of
    Left a  -> fa a
    Right b -> fb b


-- Exercises: StateT
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

-- 1
instance Functor m => Functor (StateT s m) where
  -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) =
    StateT $ (fmap . fmap) g sma
    where g (a,s) = (f a, s)

-- 2
instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (a, s))
  (StateT smfab) <*> (StateT sma) =
    StateT $ \s -> do
      (fab, s1) <- smfab s
      (a, s2) <- sma s1
      return (fab a, s2)

-- 3
instance Monad m => Monad (StateT s m) where
  return = pure

  (StateT smfab) >>= g =
    StateT $ \s -> do
      (a, s1) <- smfab s
      let (StateT smb) = g a
      smb s1


-- Exercise: Wrap It Up
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a )}

newtype ExceptT e m a =
  ExceptT { runExceptT :: m (Either e a) }

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ return <$>  (const (Right (Just 1)))


-- Exercises: Lift More
-- 1
instance MonadTrans (EitherT a) where
  lift = EitherT . liftM Right

-- 2
instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)


-- Exercises: Some Instances
-- class (Monad m) => MonadIO m where
--   liftIO :: IO a -> m a
  
newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO


-- 1. MaybeT
instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  -- (>>=) :: MaybeT m a
  --       -> (a -> MaybeT m b)
  --       -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y) 

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
-- liftIO :: IO a -> m a
  liftIO = lift . liftIO


-- 2. ReaderT
instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
-- (>>=) :: ReaderT r m a
--       -> (a -> ReaderT r m b)
--       -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

instance MonadTrans (ReaderT r) where
-- lift :: Monad m => m a -> t m a
  lift ma =
    ReaderT $ \r -> do
      a <- ma
      return a
  
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO


-- 3. StateT
instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
