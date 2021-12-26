module B.Stream where

import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Applicative ()

data Stream f m r
  = Step !(f (Stream f m r))
  | Effect (m (Stream f m r))
  | Return r

instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap f = go
    where
      go (Return r) = Return (f r)
      go (Effect m) = Effect (do go <$> m)
      go (Step g) = Step (fmap go g)

  a <$ stream = go stream
    where
      go (Return _) = Return a
      go (Effect m) = Effect (do go <$> m)
      go (Step f) = Step (fmap go f)

instance (Functor f, Monad m) => Monad (Stream f m) where
  return = pure

  stream >>= f = go stream
    where
      go (Step g) = Step (fmap go g)
      go (Effect m) = Effect (fmap go m)
      go (Return r) = f r

instance (Functor f, MonadFail m) => MonadFail (Stream f m) where
  fail = lift . Fail.fail

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  sf <*> sx = do
    f <- sf
    f <$> sx
  sf *> ss = go sf
    where
      go (Return _) = ss
      go (Effect m) = Effect (fmap go m)
      go (Step f) = Step (fmap go f)

instance Functor f => MonadTrans (Stream f) where
    lift = Effect . fmap Return