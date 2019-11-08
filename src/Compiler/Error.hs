{-# LANGUAGE ExistentialQuantification #-}
module Compiler.Error where

import Pretty

-- | Things which the compiler can omit as errors or warnings
class Pretty e => CompileError e where

data CErr = forall e. CompileError e => CErr e
