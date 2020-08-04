{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}

-- | Indexed applicative functors and monads: see 'Apply', 'Bind', 'Cobind'.

module Data.Functor.Indexed (module Data.Functor.Indexed, pure, copure) where

import Prelude (Functor (fmap), pure, (<$>), Foldable, Traversable, Eq, Ord)
import qualified Control.Applicative as Base
import Control.Category
import qualified Control.Monad as Base
import Control.Comonad (copure)
import qualified Control.Comonad as Base
import Data.Function (flip)
import Data.Kind (Type)

infixl 4 <*>, *>, <*, <**>
-- | Functors into which binary (and thus @n@-ary) functions can be lifted
--
-- Laws:
--
-- * @('.') '<$>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
--
-- Relations of methods:
--
-- * @'liftA2' f x y = f '<$>' x '<*>' y@
-- * @('<*>') = 'liftA2' 'id'@
-- * @('*>') = 'liftA2' ('pure' 'id')@
-- * @('<*') = 'liftA2' ('id' 'pure')@
class (∀ i j . Functor (p i j)) => Apply p where
    {-# MINIMAL (<*>) | liftA2 #-}

    (<*>) :: p i j (a -> b) -> p j k a -> p i k b
    (<*>) = liftA2 id

    (*>) :: p i j a -> p j k b -> p i k b
    (*>) = liftA2 (pure id)

    (<*) :: p i j a -> p j k b -> p i k a
    (<*) = liftA2 pure

    liftA2 :: (a -> b -> c) -> p i j a -> p j k b -> p i k c
    liftA2 f x y = f <$> x <*> y

(<**>) :: Apply p => p i j a -> p j k (a -> b) -> p i k b
(<**>) = liftA2 (flip id)

infixl 1 >>=
-- | Functors of which nested levels can be combined
--
-- Laws in terms of 'join':
--
-- * @'join' '.' 'fmap' 'join' = 'join' '.' 'join'@
--
-- Laws in terms of '>>=':
--
-- * @('>>=' f) '.' ('>>=' g) = ('>>=' ('>>=' f) '.' g)@
--
-- Relation of 'join' and '>>=':
--
-- * @'join' = ('>>=' 'id')@
-- * @('>>=' f) = 'join' '.' 'fmap' 'f'@
class Apply m => Bind m where
    {-# MINIMAL join | (>>=) #-}

    join :: m i j (m j k a) -> m i k a
    join = (>>= id)

    (>>=) :: m i j a -> (a -> m j k b) -> m i k b
    x >>= f = join (f <$> x)

apIxMonad :: (Bind m, ∀ k . Base.Applicative (m k k)) => m i j (a -> b) -> m j k a -> m i k b
apIxMonad fm xm = [f x | f <- fm, x <- xm] where
    return = Base.pure

infixr 1 <<=
-- | Dual of 'Bind'
--
-- Laws in terms of 'cut':
--
-- * @'cut' '.' 'cut' = 'fmap' 'cut' '.' 'cut'@
--
-- Laws in terms of '<<=':
--
-- * @(f '<<=') '.' (g '<<=') = (f '.' (g '<<=') '<<=')@
--
-- Relation of 'cut' and '<<=':
--
-- * @'cut' = ('id' '<<=')@
-- * @(f '<<=') = 'fmap' f . 'cut'@
class (∀ i j . Functor (ɯ i j)) => Cobind ɯ where
    {-# MINIMAL cut | (<<=) #-}

    cut :: ɯ i k a -> ɯ i j (ɯ j k a)
    cut = (<<=) id

    (<<=) :: (ɯ j k a -> b) -> ɯ i k a -> ɯ i j b
    (<<=) f = fmap f . cut

infixl 1 =>>
(=>>) :: Cobind ɯ => ɯ i k a -> (ɯ j k a -> b) -> ɯ i j b
(=>>) = flip (<<=)

infixr 1 =>=, =<=

(=>=) :: Cobind ɯ => (ɯ j k a -> b) -> (ɯ i j b -> c) -> ɯ i k a -> c
f =>= g = g . (f <<=)

(=<=) :: Cobind ɯ => (ɯ i j b -> c) -> (ɯ j k a -> b) -> ɯ i k a -> c
(=<=) = flip (=>=)

newtype IxWrap f i j a = IxWrap { unIxWrap :: f a }
  deriving (Functor)

deriving via (p :: Type -> Type) instance Base.Applicative p => Base.Applicative (IxWrap p i j)
deriving via (m :: Type -> Type) instance Base.Monad m => Base.Monad (IxWrap m i j)
instance Base.Comonad ɯ => Base.Comonad (IxWrap ɯ i j) where
    cut (IxWrap ɯ) = IxWrap (IxWrap <$> Base.cut ɯ)
    copure (IxWrap ɯ) = copure ɯ

instance Base.Applicative p => Apply (IxWrap p) where
    IxWrap f <*> IxWrap x = IxWrap (f Base.<*> x)

instance Base.Monad m => Bind (IxWrap m) where
    join = IxWrap . Base.join . fmap unIxWrap . unIxWrap

instance Base.Comonad ɯ => Cobind (IxWrap ɯ) where
    cut = IxWrap . fmap IxWrap . Base.cut . unIxWrap

infixr 1 >=>, <=<, =<<

(>=>) :: Bind m => (a -> m i j b) -> (b -> m j k c) -> a -> m i k c
f >=> g = (>>= g) . f

(<=<) :: Bind m => (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
(<=<) = flip (>=>)

(=<<) :: Bind m => (a -> m j k b) -> m i j a -> m i k b
(=<<) = flip (>>=)
