{-# LANGUAGE RankNTypes #-}

module B.Stream where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans (MonadTrans (..))

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

instance (Functor f, Monad m, Semigroup w) => Semigroup (Stream f m w) where
  a <> b = do
    w <- a
    fmap (w <>) b

instance (Functor f, Monad m, Monoid w) => Monoid (Stream f m w) where
  mempty = return mempty

  mappend a b = do
    w <- a
    fmap (w `mappend`) b

instance (Applicative f, Monad m) => Alternative (Stream f m) where
  empty = never
  s1 <|> s2 = zipWith' liftA2 s1 s2

instance (Applicative f, Monad m) => MonadPlus (Stream f m) where
  mzero = empty
  mplus = (<|>)

instance Functor f => MonadTrans (Stream f) where
  lift = Effect . fmap Return

zipWith ::
  forall f g h m r.
  (Monad m, Functor h) =>
  (forall x y. f x -> g y -> h (x, y)) ->
  Stream f m r ->
  Stream g m r ->
  Stream h m r
zipWith phi = zipWith' $ \xyp fx gy -> uncurry xyp <$> phi fx gy

zipWith' ::
  forall f g h m r.
  Monad m =>
  (forall x y p. (x -> y -> p) -> f x -> g y -> h p) ->
  Stream f m r ->
  Stream g m r ->
  Stream h m r
zipWith' phi = go
  where
    go s t = case s of
      Return r -> Return r
      Step fs -> case t of
        Return r -> Return r
        Step gs -> Step $ phi go fs gs
        Effect n -> Effect $ fmap (go s) n
      Effect m -> Effect $ fmap (`go` t) m

never :: (Monad m, Applicative f) => Stream f m r
never = let fix = Step $ pure (Effect (return fix)) in fix