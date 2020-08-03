module Control.Monad.Indexed.Trans.Writer where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Monad ((>=>), MonadPlus (..))
import Control.Monad.Fix (MonadFix (..))
import Data.Functor.Indexed

newtype WriterT κ f i j a = WriterT { runWriterT :: f (a, κ i j) }
  deriving (Foldable, Functor, Traversable)

lift :: (Functor f, Category κ) => f a -> WriterT κ f k k a
lift xm = WriterT $ flip (,) id <$> xm

mapWriterT :: (f (a, κ i j) -> f' (a', κ' i' j')) -> WriterT κ f i j a -> WriterT κ' f' i' j' a'
mapWriterT f = WriterT . f . runWriterT

tell :: Applicative p => κ i j -> WriterT κ p i j ()
tell = WriterT . pure . (,) ()

listen :: Functor f => WriterT κ f i j a -> WriterT κ f i j (a, κ i j)
listen = mapWriterT . fmap $ \ (a, v) -> ((a, v), v)

pass :: Functor f => WriterT κ f i j (a, κ i j -> κ i j) -> WriterT κ f i j a
pass = mapWriterT . fmap $ \ ((a, f), v) -> (a, f v)

censor :: Functor f => (κ i j -> κ' i' j') -> WriterT κ f i j a -> WriterT κ' f i' j' a
censor = mapWriterT . fmap . fmap

instance (Applicative p, Category κ) => Applicative (WriterT κ p k k) where
    pure = ipure
    (<*>) = iap

instance (Monad m, Category κ) => Monad (WriterT κ m k k) where
    (>>=) = flip ibind

instance (Category κ, Applicative p) => IxApplicative (WriterT κ p) where
    ipure = lift . pure
    WriterT x `iap` WriterT y = WriterT $ (\ (f, u) (a, v) -> (f a, v . u)) <$> x <*> y

instance (Category κ, Monad m) => IxMonad (WriterT κ m) where
    ijoin (WriterT x) = WriterT [(b, v . u) | (WriterT y, u) <- x, (b, v) <- y]

instance (Alternative p, Category κ) => Alternative (WriterT κ p k k) where
    empty = WriterT empty
    WriterT x <|> WriterT y = WriterT (x <|> y)

instance (MonadPlus p, Category κ) => MonadPlus (WriterT κ p k k)

instance (MonadFix m, Category κ) => MonadFix (WriterT κ m k k) where
    mfix f = WriterT $ mfix (\ (a, u) -> (\ (b, v) -> (b, v . u)) <$> runWriterT (f a))

liftCallCC
 :: Category κ
 => CallCC f g h (a, κ k k) (b, κ i₁ j₁) (c, κ i₂ j₂) (d, κ i₃ j₃) -> CallCC (WriterT κ f i₁ j₁) (WriterT κ g i₂ j₂) (WriterT κ h i₃ j₃) a b c d
liftCallCC callCC f = WriterT $ callCC $ \ k -> runWriterT $ f $ \ a -> WriterT $ k (a, id)

type CallCC f g h a b c d = ((a -> f b) -> g c) -> h d

liftCatch
 :: Catch e f g h (a, κ i₁ j₁) (b, κ i₂ j₂) (c, κ i₃ j₃)
 -> Catch e (WriterT κ f i₁ j₁) (WriterT κ g i₂ j₂) (WriterT κ h i₃ j₃) a b c
liftCatch catchE (WriterT xm) h = WriterT $ catchE xm $ runWriterT . h

type Catch e f g h a b c = f a -> (e -> g b) -> h c
