{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Data.Stream where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetLine, hIsEOF, openFile, Handle, hPutStrLn, withFile)

data Stream (m :: * -> *) a where
  Yield :: m a -> Stream m a -> Stream m a
  Await :: m x -> (x -> Stream m a) -> Stream m a
  Done :: m () -> Stream m a

instance Monad m => Functor (Stream m) where
  fmap = liftM

instance Monad m => Applicative (Stream m) where
  pure = return

  (<*>) = ap

instance Monad m => Monad (Stream m) where
  return a = Yield (return a) (Done (return ()))

  (Yield ma next) >>= f = Await ma ((<> (next >>= f)) . f)
  (Await ma cont) >>= f = Await ma (cont >=> f)
  (Done close) >>= f = Done close

instance Monad m => Semigroup (Stream m a) where
  (Yield ma next) <> s = Yield ma (next <> s)
  (Await mx cont) <> s = Await mx ((<> s) . cont)
  (Done _discard) <> (Yield ma next) = Yield (_discard >> ma) next
  (Done _discard) <> (Await mx cont) = Await (_discard >> mx) cont
  (Done _discard) <> (Done close') = Done (_discard >> close')

instance Monad m => Monoid (Stream m a) where
  mempty = Done (return ())
  mappend = (<>)

fromList :: Monad m => [a] -> Stream m a
fromList = foldr (Yield . return) mempty

fromStdIn :: Stream IO String
fromStdIn = Await getLine return

fromFile :: FilePath -> Stream IO String
fromFile file = Await handle cont
  where
    handle :: IO (Handle, Bool)
    handle = openFile file ReadMode >>= handleStatus

    handleStatus :: Handle -> IO (Handle, Bool)
    handleStatus h = fmap (h,) (hIsEOF h)

    cont :: (Handle, Bool) -> Stream IO String
    cont (h, True) = Done (hClose h)
    cont (h, False) = Yield (hGetLine h) (Await (handleStatus h) cont)

toFile :: FilePath -> Stream IO String -> IO ()
toFile file stream = withFile file WriteMode $ \h -> drain . mapF (hPutStrLn h) $ stream

toList :: Monad m => Stream m a -> m [a]
toList (Yield ma next) = do
  h <- ma
  t <- toList next
  return (h : t)
toList (Await mx cont) = do
  x <- mx
  toList (cont x)
toList (Done discard) = discard >> return []

drain :: Monad m => Stream m a -> m ()
drain (Yield ma next) = ma >> drain next
drain (Await mx cont) = mx >>= (drain . cont)
drain (Done close) = close

mapF :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapF f (Yield ma next) = Yield (ma >>= f) (mapF f next)
mapF f (Await mx cont) = Await mx (mapF f . cont)
mapF f (Done close) = Done close

fold :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
fold f b (Yield ma next) = do
  b' <- fmap (f b) ma
  fold f b' next
fold f b (Await mx cont) = do
  x <- mx
  fold f b (cont x)
fold _ b (Done dicard) = dicard >> return b

reduce :: (Monad m, Monoid a) => Stream m a -> m a
reduce = fold (<>) mempty