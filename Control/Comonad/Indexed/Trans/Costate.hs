-- | The indexed costate (also called "store") transfomer: each @'CostateT' f i j@ term has a value of type @i@, and wants within @f@ a value of type @j@.

module Control.Comonad.Indexed.Trans.Costate where

import Prelude hiding ((.))
import qualified Control.Applicative as Base
import qualified Control.Comonad as Base
import Control.Semigroupoid
import Data.Functor.Indexed

data CostateT f i j a = CostateT (f (j -> a)) i
  deriving (Functor)

colift :: Functor f => CostateT f k k a -> f a
colift (CostateT x s) = ($ s) <$> x

lens :: Functor φ => ((f (j -> a), i) -> φ (g (v -> b), u)) -> CostateT f i j a -> φ (CostateT g u v b)
lens φ (CostateT x s) = uncurry CostateT <$> φ (x, s)

instance Base.Applicative p => Apply (CostateT p) where
    liftA2 φ (CostateT x i) (CostateT y j) = CostateT (Base.liftA2 (\ f g -> φ (f j) . g) x y) i

instance (Base.Comonad ɯ) => Cobind (CostateT ɯ) where
    cut (CostateT f i) = CostateT (CostateT f <$ f) i

instance (Base.Comonad ɯ) => Base.Comonad (CostateT ɯ k k) where
    copure (CostateT f i) = copure f i
    cut = cut
