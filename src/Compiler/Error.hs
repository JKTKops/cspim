{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
module Compiler.Error where

import Pretty

import Compiler.Flags

import System.Console.ANSI

-- | Things which the compiler can omit as errors or warnings
class Pretty e => CompileError e where
    -- | Check if a given flag should turn this error from a warning to an error
    --   or vice versa. CompileError may safely assume that returning an action
    --   here /will/ cause that action to be executed, if it affects their pretty instance.
    flagAffects :: Flag -> e -> Maybe CEAction


data CEAction = W2Error | E2Warning deriving (Eq, Show)

data ErrorType = Warning | Error deriving (Eq, Show, Enum, Bounded)
data CErr = forall e. CompileError e => CErr ErrorType e

-- | Construct a CErr without specifying a warning/error type.
--   Attempting to force this CErr will crash - let compileErrors/Warnings update it!
mkCErr :: CompileError e => e -> CErr
mkCErr = CErr undefined

mkCError :: CompileError e => e -> CErr
mkCError = CErr Error
mkCWarning :: CompileError e => e -> CErr
mkCWarning = CErr Warning

isWarning (CErr Warning _) = True
isWarning _ = False

isError (CErr Error _) = True
isError _ = False

instance Pretty CErr where
    pretty = prettyColoredCErr

prettyColoredCErr = \case
    CErr Warning e ->
        reset ++ "[" ++ purple ++ "Warning" ++ reset ++ "]: " ++ pretty e
    CErr Error e ->
        reset ++ "[" ++ red ++ "Error" ++ reset ++ "]: " ++ pretty e
  where reset  = setSGRCode [Reset]
        purple = setSGRCode [SetPaletteColor Foreground $ xterm6LevelRGB 2 2 5]
        red    = setSGRCode [SetPaletteColor Foreground $ xterm6LevelRGB 5 0 0]
