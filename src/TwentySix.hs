module TwentySix where


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
