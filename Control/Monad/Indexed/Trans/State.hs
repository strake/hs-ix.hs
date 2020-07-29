module Control.Monad.Indexed.Trans.State where

import Control.Applicative
import Control.Monad ((>=>), MonadPlus (..))
import Control.Monad.Fix (MonadFix (..))
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

liftCallCC
 :: CallCC f g h (a, i) (b, j) (c, k) (d, l)
 -> CallCC (StateT f e j) (StateT g i k) (StateT h i l) a b c d
liftCallCC callCC f =
    StateT $ \ st ->
    callCC $ \ k ->
    runStateT (f $ \ a -> StateT $ \ _ -> k (a, st)) st

type CallCC f g h a b c d = ((a -> f b) -> g c) -> h d

liftCatch
 :: Catch e f g h (a, i) (b, j) (c, k)
 -> Catch e (StateT f l i) (StateT g l j) (StateT h l k) a b c
liftCatch catchE (StateT xm) h = StateT $ \ st -> xm st `catchE` \ e -> runStateT (h e) st

type Catch e f g h a b c = f a -> (e -> g b) -> h c

liftListen
 :: Functor f
 => Listen w f (a, j) b
 -> Listen w (StateT f i j) a (i -> b)
liftListen listen xm = StateT $ \ st ->
    flip fmap (listen (xm st)) $ \ ~((a, st'), w) -> ((a, w), st')

type Listen w f a b = b -> f (a, w)

liftPass
 :: Functor f
 => Pass z f g (a, k) (b, j)
 -> Pass z (StateT f i k) (StateT g i j) a b
liftPass pass (StateT xm) = StateT $ \ st ->
    pass $ flip fmap (xm st) $ \ ~((a, f), st') -> ((a, st'), f)

type Pass z f g a b = f (a, z) -> g b
