{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
module Compiler.Error where

import Pretty

import Compiler.Flags

import System.IO (hPutStrLn, stderr) -- only to write printCErrs
import System.Console.ANSI

-- | Things which the compiler can omit as errors or warnings
class Pretty e => CompileError e where
    -- | Check if a given flag should turn this error from a warning to an error
    --   or vice versa.
    flagAffects :: Flag -> e -> CEAction

data CEAction = W2Error | E2Warning | Ignore | NoChange deriving (Eq, Show)

-- | The types of errors inside cspim. 'VerboseLog' should be touched /as little as possible/,
--   you can pattern match on it if necessary, but only use it to construct data
--   if you really know what you're doing.
data ErrorType = VerboseLog | Warning | Error deriving (Eq, Show, Enum, Bounded)
data CErr = forall e. CompileError e => CErr ErrorType e

newtype VerboseLog = VL { getLog :: String }
instance Pretty VerboseLog where
    pretty (VL s) = s
instance CompileError VerboseLog where
    flagAffects _ _ = NoChange

newtype PanicErr = Panic String
instance Pretty PanicErr where
    pretty (Panic s) = "Panic! " ++ s
instance CompileError PanicErr where
    flagAffects _ _ = NoChange

-- | Construct a CErr without specifying a warning/error type.
--   Attempting to force this CErr will crash - let compileErrors/Warnings update it!
mkCErr :: CompileError e => e -> CErr
mkCErr = CErr undefined

mkCError :: CompileError e => e -> CErr
mkCError = CErr Error
mkCWarning :: CompileError e => e -> CErr
mkCWarning = CErr Warning

isVerboseLog (CErr VerboseLog _) = True
isVerboseLog _ = False

isWarning (CErr Warning _) = True
isWarning _ = False

isError (CErr Error _) = True
isError _ = False

removeVerboseLogs :: [CErr] -> [CErr]
removeVerboseLogs = filter (\(CErr t _) -> t /= VerboseLog)

instance Pretty CErr where
    pretty = prettyColoredCErr

prettyColoredCErr = \case
    CErr VerboseLog e ->
        reset ++ "[" ++ aqua ++ "--verbose" ++ reset ++ "]: " ++ pretty e
    CErr Warning e ->
        reset ++ "[" ++ purple ++ "Warning" ++ reset ++ "]: " ++ pretty e
    CErr Error e ->
        reset ++ "[" ++ red ++ "Error" ++ reset ++ "]: " ++ pretty e
  where reset  = setSGRCode [Reset]
        aqua   = setSGRCode [SetPaletteColor Foreground $ xterm6LevelRGB 0 5 5]
        purple = setSGRCode [SetPaletteColor Foreground $ xterm6LevelRGB 2 2 5]
        red    = setSGRCode [SetPaletteColor Foreground $ xterm6LevelRGB 5 0 0]

printCErrs :: [CErr] -> IO ()
printCErrs = mapM_ (hPutStrLn stderr . pretty)

instance Pretty ErrorType where
    pretty VerboseLog = "verbose log"
    pretty Warning    = "warning"
    pretty Error      = "error"
