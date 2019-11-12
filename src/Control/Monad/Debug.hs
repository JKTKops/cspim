module Control.Monad.Debug where

import Debug.Trace

-- | Useful for printf debugging inside of monads. Provide a label and a value
--   and the monadic action will log the string label ++ show value.
debugShow :: (Applicative f, Show a) => String -> a -> f ()
debugShow tag val = traceM $ tag ++ show val

-- | synonym for 'traceM'
debug :: Applicative f => String -> f ()
debug = traceM
