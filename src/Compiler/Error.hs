{-# LANGUAGE ExistentialQuantification #-}
module Compiler.Error where

import Pretty

import Compiler.Flags

-- | Things which the compiler can omit as errors or warnings
class Pretty e => CompileError e where
    -- | Check if a given flag should turn this error from a warning to an error
    --   or vice versa.
    flagAffects :: Flag -> e -> Maybe CEAction

data CEAction = W2Error | E2Warning deriving (Eq, Show)

data CErr = forall e. CompileError e => CErr e
