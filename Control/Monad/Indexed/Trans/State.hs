module Control.Monad.Indexed.Trans.State where

import Control.Applicative
import Control.Monad ((>=>), MonadPlus (..))
import Control.Monad.Fix (MonadFix (..))
import Data.Functor.Indexed

newtype StateT f i j a = StateT { runStateT :: i -> f (a, j) }
  deriving (Functor)

mapStateT :: (f (a, j) -> g (b, k)) -> StateT f i j a -> StateT g i k b
mapStateT f (StateT x) = StateT (f . x)

modify :: Applicative p => (i -> j) -> StateT p i j i
modify f = modifyF (pure . f)

modifyF :: Functor f => (i -> f j) -> StateT f i j i
modifyF = StateT . liftA2 fmap (,)

get :: Applicative p => StateT p k k k
get = modifyF pure

put :: Applicative p => j -> StateT p i j ()
put = StateT . pure . pure . (,) ()

instance Monad m => IxApplicative (StateT m) where
    ipure a = StateT $ pure . (,) a
    StateT fm `iap` StateT xm = StateT $ \ i -> [(f x, k) | (f, j) <- fm i, (x, k) <- xm j]

instance Monad m => IxMonad (StateT m) where
    ijoin = StateT . (>=> uncurry runStateT) . runStateT

instance Monad m => Applicative (StateT m k k) where
    pure = ipure
    (<*>) = iap

instance Monad m => Monad (StateT m k k) where
    (>>=) = flip ibind

instance MonadPlus m => Alternative (StateT m k k) where
    empty = StateT (pure empty)
    StateT a <|> StateT b = StateT (liftA2 (<|>) a b)

instance MonadPlus m => MonadPlus (StateT m k k) where
    mzero = empty
    mplus = (<|>)

instance MonadFix m => MonadFix (StateT m k k) where
    mfix f = StateT $ mfix . \ k -> flip runStateT k . f . fst
