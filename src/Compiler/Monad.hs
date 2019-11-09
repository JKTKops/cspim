{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Compiler.Monad
    ( module Compiler.Monad.Class
    , module Compiler.Monad
    )where

import Compiler.Monad.Class hiding (runCompilerMonad)
import qualified Compiler.Monad.Class as FMC

import Compiler.Flags
import Compiler.Error

import Data.DList
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Bifunctor

import Control.Monad.Reader
import Control.Monad.Validate
import qualified Control.Monad.Validate.Internal as V

-- | Converts all warnings in the current computation to errors.
--   Note that the monad-validate library makes it impossible to
--   write a function which /ignores/ all warnings in the
--   current computation. For that, see 'unwarnC', which requires
--   round-tripping.
werror :: MonadTrans t => t Compiler ()
werror = lift $ C $ lift $ V.validateT $ \case
    V.MNothing -> return $ Right (V.MNothing, ())
    V.MJust es -> return $ Left es

withTheseC :: FullMonadCompiler m => (These (DList CErr) a -> m b) -> m a -> m b
withTheseC f m = runMonadCompiler m >>= f
{-# INLINE withTheseC #-}

-- | Like withTheseC, but lets you give functions for each case rather than for the whole These.
--   A common idiom is presumed to be 'handleC' error warn ok where ...
handleC :: FullMonadCompiler m
        => (DList CErr -> m b)
        -> (DList CErr -> a -> m b)
        -> (a -> m b)
        -> m a
        -> m b
handleC this these that = withTheseC $ \case
    This e -> this e
    That a -> that a
    These e a -> these e a

-- | Takes a compiler action and causes any warnings it emits to become fatal errors.
werrorC :: FullMonadCompiler m => m a -> m a
werrorC = handleC error warn ok
  where error = compilerErrors
        warn e _ = compilerErrors e
        ok = return

-- | Takes a compiler action and ignores any warnings it emits.
unwarnC :: FullMonadCompiler m => m a -> m a
unwarnC = handleC error warn ok
  where error = compilerErrors
        warn _ a = return a
        ok = return
