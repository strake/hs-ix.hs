-- | The indexed constant functor: a @'Const' κ@ ignores its final argument and merely holds a morphism of @κ@, which are composed as the @'Const'@ terms are 'join'ed and @('<*>')@d.

module Data.Functor.Indexed.Const (Const (..)) where

import Prelude hiding (Applicative (..), (.), id)
import qualified Control.Applicative as Base
import Control.Category (Category (id))
import Control.Semigroupoid
import Data.Functor.Indexed

newtype Const κ a b z = Const { getConst :: κ a b }
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Semigroupoid κ => Apply (Const κ) where
    Const f <*> Const g = Const (g . f)

instance Category κ => Base.Applicative (Const κ a a) where
    pure = pure (Const id)
    (<*>) = (<*>)
