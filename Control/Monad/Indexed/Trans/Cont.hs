-- | The indexed continuation transformer: see 'ContT'.

module Control.Monad.Indexed.Trans.Cont where

import Prelude (Functor (..), flip, ($), (<$>))
import Control.Applicative (Alternative (..))
import qualified Control.Applicative as Base
import Control.Category
import qualified Control.Monad as Base
import qualified Control.Monad.Fail as Base
import Data.Functor.Indexed

newtype ContT f i j a = ContT { runContT :: (a -> f j) -> f i }
  deriving (Functor)

lift :: Base.Monad m => m a -> ContT m i i a
lift = ContT . (Base.>>=)

evalContT :: Base.Applicative p => ContT p a a a -> p a
evalContT = flip runContT pure

mapContT :: (f i -> f j) -> ContT f i k a -> ContT f j k a
mapContT φ (ContT f) = ContT (φ . f)

withContT :: ((b -> f j) -> (a -> f k)) -> ContT f i k a -> ContT f i j b
withContT φ (ContT f) = ContT (f . φ)

callCC :: ((a -> ContT f j k b) -> ContT f i j a) -> ContT f i j a
callCC f = ContT $ \ k -> runContT (f $ ContT . pure . k) k

resetT :: Base.Monad m => ContT m a i i -> ContT m j j a
resetT (ContT f) = ContT (f pure Base.>>=)

shiftT :: Base.Monad m => ((a -> m j) -> ContT m i k k) -> ContT m i j a
shiftT f = ContT (flip runContT pure . f)

instance Apply (ContT f) where
    (<*>) = apIxMonad

instance Bind (ContT f) where
    join (ContT f) = ContT $ f . flip runContT

instance Base.Applicative (ContT f k k) where
    pure = ContT . flip id
    (<*>) = (<*>)

instance Alternative p => Alternative (ContT p k k) where
    empty = ContT $ pure empty
    ContT f <|> ContT g = ContT $ Base.liftA2 (<|>) f g

instance Base.Monad (ContT f k k) where
    (>>=) = (>>=)

instance Alternative p => Base.MonadPlus (ContT p k k) where
    mzero = empty
    mplus = (<|>)

instance Base.MonadFail m => Base.MonadFail (ContT m k k) where
    fail = ContT . pure . Base.fail

liftLocal
 :: (Base.Monad m, Base.Applicative p)
 => m r -> (p r -> m i -> m j) -> p r -> ContT m i j a -> ContT m j i a
liftLocal ask local f (ContT xm) = ContT $ \ k -> do
    g <- pure <$> ask
    local f (xm (local g . k))
