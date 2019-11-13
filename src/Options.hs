{-|
This module handles cspim's systems for reading command line arguments and deciding what they
mean. It is based on System.Console.GetOpt.

We distinguish between two types of options.

The first kind is the /one-shot/ options. These
options cause cspim to take a single, simple, one-shot action and then immediately exit
successfully. These are options such as --version, --help, and --show-options.

The second kind are the /compilation/ options. These options fall through into the actual
pipeline, causing selection of initial conditions, like input files, the output file, and
the verbosity; or actually affecting the progress of compilation, like deferring certain
types of errors and turning them into warnings, or turning certain classes of warnings
into errors.
-}
{-# LANGUAGE TemplateHaskell #-}
module Options
    ( Options(..)
    , parseArgs

    , inputFiles
    , outputFile
    , flags
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad (when)

import Compiler.Flags
import Paths_cspim (version)
import Data.Version (showVersion)

import System.Exit
import System.Environment
import System.Console.GetOpt


-- | Options supported by the CSpim compiler which need to advance into the actual
--   compilation system. One-shot options, like --help, are handled automatically.
data Options = Options
    { _inputFiles :: [FilePath]
    , _outputFile :: Maybe FilePath
    , _flags      :: Flags
    }
  deriving (Show)

-- | The type of options internal to this module, which includes the additional
--   one-shot options.
data InternalOptions = InternalOpts
    { _options        :: Options
    , _optShowVersion :: Bool
    , _optShowHelp    :: Bool
    , _optShowOptions :: Bool
    }
  deriving Show

makeLenses ''Options
makeLenses ''InternalOptions

-- | The starting point for options handling; each options is parsed into a function
--   f : InternalOptions -> InternalOptions and then they are all processed in sequence.
defaultOptions = InternalOpts
    { _options = Options
        { _inputFiles     = []
        , _outputFile     = Nothing
        , _flags          = noFlags
        }
    , _optShowVersion = False
    , _optShowHelp    = False
    , _optShowOptions = False
    }

-- | A description of an option. Each option is identified with a function that modifies
--   a given InternalOptions record. The command line flags are combined by executing
--   the corresponding functions from left-to-right.
type OptDesc = OptDescr (InternalOptions -> InternalOptions)

-- | The basic, common flags which should be displayed by the usage text.
--   Most flags belong under advancedOptions, which will only be shown by the
--   "--show-options" one-shot flag.
basicOptions :: [OptDesc]
basicOptions =
    [ Option ['v'] ["verbose"] (NoArg $ options.flags %~ setFlag Verbose)
        "be verbose about what's happening on stderr, currently ignored"

    , Option ['V'] ["version"] (NoArg $ optShowVersion .~ True)
        "show version information and exit"

    , Option ['?'] ["help"]    (NoArg $ optShowHelp .~ True)
        "show a help text and exit"

    , Option ['o'] ["output"]  (ReqArg (options.outputFile ?~) "FILE")
        "direct the compiler output to the given file"
    ]

-- | The description for --show-options.
showOptionsOption :: OptDesc
showOptionsOption = Option [] ["show-options"] (NoArg $ optShowOptions .~ True)
    "show all cspim options and exit"


-- | Advanced options which are not displayed by the usage text.
--   These options are displayed (along with the basic ones) by "--show-options".
advancedOptions :: [OptDesc]
advancedOptions =
    [ Option [] ["f-defer-out-of-scope-errors"]
        (NoArg $ options.flags %~ setFlag FDeferOutOfScopeErrors)
        "don't crash the parser if it finds an out-of-scope variable, \
        \useful along with -ddump-tac"

    , Option [] ["fno-defer-out-of-scope-errors"]
        (NoArg $ options.flags %~ unsetFlag FDeferOutOfScopeErrors)
        "disable f-defer-out-of-scope-errors"
    ]

allOptions = basicOptions ++ [showOptionsOption] ++ advancedOptions

-- | The "usage information" string to be printed with unrecognized options or --help.
info :: String
info = usageInfo header basicOptions
  where header = "Usage: cspim [OPTION...] FILE"


parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    opts <- case getOpt' Permute allOptions argv of
        (o, files, [], []) -> return $ foldl (&) defaultOptions o
                                     & options.inputFiles .~ files
        (_, _, unrecog, []) -> do
            putStrLn $ "Unrecognized options: " ++ unwords unrecog ++ "\n"
                     ++ info
            exitWith (ExitFailure 1)
        (_, _, _, errs) -> do
            putStrLn $ concat errs ++ info
            exitWith (ExitFailure 1)

    handleOneShotOpts opts
    return $ opts ^. options

handleOneShotOpts :: InternalOptions -> IO ()
handleOneShotOpts InternalOpts{ _optShowVersion = ver
                              , _optShowHelp = help
                              , _optShowOptions = opts} = do
    when ver  showVer
    when help showHelp
    when opts showOpts
    when (ver || help || opts) exitSuccess

  where showVer  = putStrLn $ "cspim v" ++ showVersion version
                           ++ " - Max Kopinsky & Ajay Tatachar (C) 2019"

        showHelp = putStrLn $ info ++ "\ncspim has more options. \
                             \If you really want to see them all,\n\
                             \pass --show-options to the compiler."
        showOpts = putStrLn $ usageInfo "" allOptions
