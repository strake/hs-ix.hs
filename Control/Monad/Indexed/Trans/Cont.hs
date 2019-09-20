module Control.Monad.Indexed.Trans.Cont where

import Prelude (Functor (..), flip, ($))
import Control.Applicative
import Control.Category
import Control.Monad (Monad ((>>=)), MonadPlus (..))
import Control.Monad.Fail (MonadFail (..))
import Data.Functor.Indexed

newtype ContT f i j a = ContT { runContT :: (a -> f j) -> f i }
  deriving (Functor)

lift :: Monad m => m a -> ContT m i i a
lift = ContT . (>>=)

evalContT :: Applicative p => ContT p a a a -> p a
evalContT = flip runContT pure

mapContT :: (f i -> f j) -> ContT f i k a -> ContT f j k a
mapContT φ (ContT f) = ContT (φ . f)

withContT :: ((b -> f j) -> (a -> f k)) -> ContT f i k a -> ContT f i j b
withContT φ (ContT f) = ContT (f . φ)

callCC :: ((a -> ContT f j k b) -> ContT f i j a) -> ContT f i j a
callCC f = ContT $ \ k -> runContT (f $ ContT . pure . k) k

resetT :: Monad m => ContT m a i i -> ContT m j j a
resetT (ContT f) = ContT (f pure >>=)

shiftT :: Monad m => ((a -> m j) -> ContT m i k k) -> ContT m i j a
shiftT f = ContT (flip runContT pure . f)

instance IxApplicative (ContT f) where
    ipure = ContT . flip id
    iap = iapIxMonad

instance IxMonad (ContT f) where
    ijoin (ContT f) = ContT $ f . flip runContT

instance Applicative (ContT f k k) where
    pure = ipure
    (<*>) = iap

instance Alternative p => Alternative (ContT p k k) where
    empty = ContT $ pure empty
    ContT f <|> ContT g = ContT $ liftA2 (<|>) f g

instance Monad (ContT f k k) where
    (>>=) = flip ibind

instance Alternative p => MonadPlus (ContT p k k) where
    mzero = empty
    mplus = (<|>)

instance MonadFail m => MonadFail (ContT m k k) where
    fail = ContT . pure . fail 

liftLocal
 :: (Monad m, Applicative p)
 => m r -> (p r -> m i -> m j) -> p r -> ContT m i j a -> ContT m j i a
liftLocal ask local f (ContT xm) = ContT $ \ k -> do
    g <- pure <$> ask
    local f (xm (local g . k))
