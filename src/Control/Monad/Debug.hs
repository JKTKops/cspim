module Control.Monad.Debug where

import Debug.Trace

-- | Useful for printf debugging inside of monads. Provide a label and a value
--   and the monadic action will log the string label ++ show value.
debug :: (Applicative f, Show a) => String -> a -> f ()
debug tag val = traceM $ tag ++ show val
