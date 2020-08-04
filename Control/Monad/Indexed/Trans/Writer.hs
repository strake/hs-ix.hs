-- | The indexed writer transformer: each @'WriterT' κ f@ term bears a morphism of @κ@ atop its argument, which are composed as the 'WriterT' terms are 'join'ed and @('<*>')@d.

module Control.Monad.Indexed.Trans.Writer where

import Prelude hiding ((.), id, (<*>), Monad (..))
import Control.Applicative (Alternative (..))
import qualified Control.Applicative as Base
import Control.Category (Category (id))
import Control.Semigroupoid (Semigroupoid (..))
import qualified Control.Monad as Base
import qualified Control.Monad.Fix as Base
import Control.Monad.Indexed.Signatures
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

instance (Base.Applicative p, Category κ) => Base.Applicative (WriterT κ p k k) where
    pure = lift . pure
    (<*>) = (<*>)

instance (Base.Monad m, Category κ) => Base.Monad (WriterT κ m k k) where
    (>>=) = (>>=)

instance (Semigroupoid κ, Base.Applicative p) => Apply (WriterT κ p) where
    WriterT x <*> WriterT y = WriterT $ (\ (f, u) (a, v) -> (f a, v . u)) <$> x Base.<*> y

instance (Semigroupoid κ, Base.Monad m) => Bind (WriterT κ m) where
    join (WriterT x) = WriterT [(b, v . u) | (WriterT y, u) <- x, (b, v) <- y]

instance (Alternative p, Category κ) => Alternative (WriterT κ p k k) where
    empty = WriterT empty
    WriterT x <|> WriterT y = WriterT (x <|> y)

instance (Base.MonadPlus p, Category κ) => Base.MonadPlus (WriterT κ p k k)

instance (Base.MonadFix m, Category κ) => Base.MonadFix (WriterT κ m k k) where
    mfix f = WriterT $ Base.mfix (\ (a, u) -> (\ (b, v) -> (b, v . u)) <$> runWriterT (f a))

liftCallCC
 :: Category κ
 => CallCC f g h (a, κ k k) (b, κ i₁ j₁) (c, κ i₂ j₂) (d, κ i₃ j₃) -> CallCC (WriterT κ f i₁ j₁) (WriterT κ g i₂ j₂) (WriterT κ h i₃ j₃) a b c d
liftCallCC callCC f = WriterT $ callCC $ \ k -> runWriterT $ f $ \ a -> WriterT $ k (a, id)

liftCatch
 :: Catch e f g h (a, κ i₁ j₁) (b, κ i₂ j₂) (c, κ i₃ j₃)
 -> Catch e (WriterT κ f i₁ j₁) (WriterT κ g i₂ j₂) (WriterT κ h i₃ j₃) a b c
liftCatch catchE (WriterT xm) h = WriterT $ catchE xm $ runWriterT . h
