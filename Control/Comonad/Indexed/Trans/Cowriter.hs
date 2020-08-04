module Control.Comonad.Indexed.Trans.Cowriter where

import Prelude hiding ((.), id)
import qualified Control.Applicative as Base
import Control.Category hiding ((.))
import qualified Control.Comonad as Base
import qualified Control.Monad as Base
import Control.Semigroupoid
import Data.Functor.Indexed

newtype CowriterT κ f i j a = CowriterT { runCowriterT :: f (κ i j -> a) }
  deriving (Functor)

instance (Base.Comonad ɯ, Category κ) => Base.Comonad (CowriterT κ ɯ k k) where
    copure = cotell id
    cut = cut

instance (Base.Comonad ɯ, Semigroupoid κ) => Cobind (CowriterT κ ɯ) where
    cut = mapCowriterT (Base.=>> \ ɯ s -> CowriterT $ (. (. s)) <$> ɯ)

mapCowriterT :: (f (κ i j -> a) -> g (κ' u v -> b)) -> CowriterT κ f i j a -> CowriterT κ' g u v b
mapCowriterT f = CowriterT . f . runCowriterT

cotell :: Base.Comonad ɯ => κ i j -> CowriterT κ ɯ i j a -> a
cotell κ = ($ κ) . copure . runCowriterT

listen :: Functor f => CowriterT κ f i j a -> CowriterT κ f i j (a, κ i j)
listen = (mapCowriterT . fmap) (Base.>>= (,))

censor :: Functor f => (κ' u v -> κ i j) -> CowriterT κ f i j a -> CowriterT κ' f u v a
censor = mapCowriterT . fmap . flip (.)
