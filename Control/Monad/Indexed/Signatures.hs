module Control.Monad.Indexed.Signatures where

type CallCC f g h a b c d = ((a -> f b) -> g c) -> h d

type Catch e f g h a b c = f a -> (e -> g b) -> h c

type Listen w f a b = b -> f (a, w)

type Pass z f g a b = f (a, z) -> g b
