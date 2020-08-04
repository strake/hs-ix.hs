-- | The indexed state transformer: each @'StateT' _ i j@ term takes an input of type @i@ and gives an output of type @j@.

module Control.Monad.Indexed.Trans.State where

import Prelude hiding ((<*>), Monad (..))
import Control.Applicative (Alternative (..))
import qualified Control.Applicative as Base
import qualified Control.Monad as Base
import qualified Control.Monad.Fix as Base
import Control.Monad.Indexed.Signatures
import Data.Functor.Indexed

newtype StateT f i j a = StateT { runStateT :: i -> f (a, j) }
  deriving (Functor)

lift :: Functor f => f a -> StateT f k k a
lift xm = StateT $ \ k -> flip (,) k <$> xm

mapStateT :: (f (a, j) -> g (b, k)) -> StateT f i j a -> StateT g i k b
mapStateT f (StateT x) = StateT (f . x)

modify :: Applicative p => (i -> j) -> StateT p i j i
modify f = modifyF (pure . f)

modifyF :: Functor f => (i -> f j) -> StateT f i j i
modifyF = StateT . Base.liftA2 fmap (,)

get :: Applicative p => StateT p k k k
get = modifyF pure

put :: Applicative p => j -> StateT p i j ()
put = StateT . pure . pure . (,) ()

instance Base.Monad m => Apply (StateT m) where
    StateT fm <*> StateT xm = StateT $ \ i -> [(f x, k) | (f, j) <- fm i, (x, k) <- xm j]

instance Base.Monad m => Bind (StateT m) where
    join = StateT . (Base.>=> uncurry runStateT) . runStateT

instance Base.Monad m => Base.Applicative (StateT m k k) where
    pure a = StateT $ pure . (,) a
    (<*>) = (<*>)

instance Base.Monad m => Base.Monad (StateT m k k) where
    (>>=) = (>>=)

instance Base.MonadPlus m => Alternative (StateT m k k) where
    empty = StateT (pure empty)
    StateT a <|> StateT b = StateT (Base.liftA2 (<|>) a b)

instance Base.MonadPlus m => Base.MonadPlus (StateT m k k) where
    mzero = empty
    mplus = (<|>)

instance Base.MonadFix m => Base.MonadFix (StateT m k k) where
    mfix f = StateT $ Base.mfix . \ k -> flip runStateT k . f . fst

liftCallCC
 :: CallCC f g h (a, i) (b, j) (c, k) (d, l)
 -> CallCC (StateT f e j) (StateT g i k) (StateT h i l) a b c d
liftCallCC callCC f =
    StateT $ \ st ->
    callCC $ \ k ->
    runStateT (f $ \ a -> StateT $ \ _ -> k (a, st)) st

liftCatch
 :: Catch e f g h (a, i) (b, j) (c, k)
 -> Catch e (StateT f l i) (StateT g l j) (StateT h l k) a b c
liftCatch catchE (StateT xm) h = StateT $ \ st -> xm st `catchE` \ e -> runStateT (h e) st

liftListen
 :: Functor f
 => Listen w f (a, j) b
 -> Listen w (StateT f i j) a (i -> b)
liftListen listen xm = StateT $ \ st ->
    flip fmap (listen (xm st)) $ \ ~((a, st'), w) -> ((a, w), st')

liftPass
 :: Functor f
 => Pass z f g (a, k) (b, j)
 -> Pass z (StateT f i k) (StateT g i j) a b
liftPass pass (StateT xm) = StateT $ \ st ->
    pass $ flip fmap (xm st) $ \ ~((a, f), st') -> ((a, st'), f)
