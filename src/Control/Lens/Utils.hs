module Control.Lens.Utils where

import Data.Semigroup (Any)
import Control.Lens
import Control.Monad.State

send :: MonadState s m => Getting a s a -> (a -> m b) -> m b
send g f = use g >>= f

(~>) :: MonadState s m => Getting a s a -> (a -> m b) -> m b
(~>) = send
infixr 2 ~>

hasM :: MonadState s1 m => Getting s2 s1 s2 -> Getting Any s2 a -> m Bool
hasM getter prism = getter ~> \g -> pure (has prism g)

hasn'tM getter = fmap not . hasM getter
