{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}

module Data.Functor.Indexed where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Comonad
import Data.Function (flip)
import Data.Kind (Type)

class (∀ i j . Functor (p i j), ∀ k . Applicative (p k k)) => IxApplicative p where
    ipure :: a -> p k k a
    iap :: p i j (a -> b) -> p j k a -> p i k b

class (IxApplicative m, ∀ k . Monad (m k k)) => IxMonad m where
    ijoin :: m i j (m j k a) -> m i k a
    ijoin = ibind id

    ibind :: (a -> m j k b) -> m i j a -> m i k b
    ibind f = ijoin . fmap f

iapIxMonad :: IxMonad m => m i j (a -> b) -> m j k a -> m i k b
iapIxMonad fm xm = [f x | f <- fm, x <- xm] where
    return = ipure
    (>>=) = flip ibind

class (∀ i j . Functor (ɯ i j)) => IxComonad ɯ where
    icut :: ɯ i k a -> ɯ i j (ɯ j k a)
    icut = icobind id

    icobind :: (ɯ j k a -> b) -> ɯ i k a -> ɯ i j b
    icobind f = fmap f . icut

newtype IxWrap f i j a = IxWrap { unIxWrap :: f a }
  deriving (Functor)

deriving via (p :: Type -> Type) instance Applicative p => Applicative (IxWrap p i j)
deriving via (m :: Type -> Type) instance Monad m => Monad (IxWrap m i j)

instance Applicative p => IxApplicative (IxWrap p) where
    ipure = IxWrap . pure
    IxWrap f `iap` IxWrap x = IxWrap (f <*> x)

instance Monad m => IxMonad (IxWrap m) where
    ijoin = IxWrap . join . fmap unIxWrap . unIxWrap

instance Comonad ɯ => IxComonad (IxWrap ɯ) where
    icut = IxWrap . fmap IxWrap . cut . unIxWrap
