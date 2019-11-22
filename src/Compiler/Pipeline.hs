{- |

This module handles the entire compiler pipeline. That includes constructing the compiler
phases and composing them according to the input file extension and stopping phase.

Since I'm not using Polysemy in this project, it's difficult to intersperse IO with
compiling actions, and modifying the Compiler monad to log the things it should dump
would be very messy. And dumping is primarily a debug action and it's /always/ one-shot,
so we do it with 'unsafePerformIO'. Similarly, we execute cpp over the input file with
'unsafePerformIO'.

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Compiler.Pipeline where
import Compiler.Pipeline.Internal

import Options
import Pretty

import Data.Maybe (fromJust, fromMaybe)

import System.IO
import System.IO.Unsafe
import System.Exit
import System.FilePath

import Compiler.Monad
import Compiler.Error
import Compiler.Flags

-- Compiler phases
import Parser.Parser (parseC)

import TAC.CodeGen (mipsCodeGenProc)
import qualified TAC.Program as TAC (Program)
import TAC.Pretty () -- pretty instances
import qualified MIPS.Language as MIPS
import MIPS.Pretty () -- pretty instances

import Control.Monad
import Control.Lens hiding ((<.>))
import Control.Lens.Utils

compile :: Options -> FilePath -> IO ()
compile opts filename =
    let ext = takeExtension filename
    in case ext of
        ""   -> do putStrLn "file with no extension given - cspim does not contain a linker."
                   exitWith (ExitFailure 1)
        ".c" -> runPipeline opts CppPhase filename
        ".i" -> runPipeline opts ParsePhase filename
        _    -> do putStrLn $ "file extension '" ++ ext ++ "' not recongized."
                   exitWith (ExitFailure 1)

runPipeline :: Options -> Phase -> FilePath -> IO ()
runPipeline opts startPhase fname = do
    source <- readFile fname
    let result = runCompiler (compileAction source) (opts^.flags)
        outFile = fromMaybe (dropExtension fname <.> outputExt) (opts^.outputFile)
    case result of
        This err    -> exitWithCompileError err
        That result -> writeFile outFile result
        These errs result -> do
            printCErrs errs
            writeFile outFile result

  where compileAction = finalizeStdErrOutput . pipeline (startPhase, endPhase) fname

        -- This is abstraction overkill currently but may not be in the future.
        -- "Pick the earliest end phase which is enabled"
        endPhase = fromJust $ msum [cppEnd, Just PrettyMipsPhase]
          where cppEnd = if isFlagSet E theFlags then Just CppPhase else Nothing
                theFlags = opts^.flags

        outputExt = case endPhase of
            PrettyMipsPhase -> "s"
            CppPhase        -> "i"

exitWithCompileError :: [CErr] -> IO ()
exitWithCompileError errs = do
    printCErrs errs
    exitWith $ ExitFailure 1

--------------------------------------------------------------------------------------
-- The pipeline
--------------------------------------------------------------------------------------

data Phase
     = CppPhase
     | ParsePhase

     | OptTacPhase
     | Tac2MipsPhase
     | OptMipsPhase

     | PrettyMipsPhase
  deriving (Eq, Ord, Show, Enum, Bounded)

pipeline :: (Phase, Phase) -> FilePath -> String -> Compiler String
pipeline phases fname = execPipe (pickPipe phases fname)

data Pipe m a b where
    End :: Pipe m a a
    (:|>) :: (a -> m b) -> Pipe m b c -> Pipe m a c
infixr 6 :|>

type PhaseDesc a b = a -> Compiler b

-- | Similar to '*>' but also passes an argument through to the center.
--   Compare:
--
-- @
-- (>*>) :: Applicative f => f a -> (t -> f b) -> t -> f b
-- (*>)  :: Applicative f => f a       -> f b       -> f b
-- @
(>*>) :: Applicative f => f a -> (t -> f b) -> t -> f b
(>*>) a f b = a *> f b
-- | Similar to '<*' but also passes an argument through to the center.
--   Compare:
-- @
-- (<*<) :: Applicative f => (t -> f a) -> f b -> t -> f a
-- (<*)  :: Applicative f =>       f a  -> f b      -> f a
-- @
(<*<) :: Applicative f => (t -> f a) -> f b -> t -> f a
(<*<) f b a = f a <* b
infixl 4 >*>, <*<

cppPhase :: PhaseDesc String String
cppPhase = pure -- todo

parsePhase :: String -> PhaseDesc String TAC.Program
parsePhase fname src = do
    verboseLog "Start parsing phase..."
    prog <- parseC fname src
    verboseLog "End parsing phase."
    verboseLog "Checking if we should dump TAC..."
    ifM (isFlagSet DumpTac <$> compilerFlags) (dumpTac prog) (verboseLog "No.")
    return prog

  where dumpTac prog = do
            verboseLog "Yes..."
            dump (dropExtension fname <.> dumpExtension DumpTac, pretty prog)
            verboseLog "Dumped TAC."

-- Note: can't dump inside optimizer since it won't dump at all if it crashes
-- (CompilerT :( ) so just dump the final output and use Hoopl fuel to debug it.
optTacPhase :: PhaseDesc TAC.Program TAC.Program
optTacPhase = pure -- TODO setup --dump-simpl

tac2MipsPhase :: PhaseDesc TAC.Program MIPS.Program
tac2MipsPhase = verboseLog "Start instruction selection..."
            >*> mipsCodeGenProc
            <*< verboseLog "Done selecting instructions."

optMipsPhase :: PhaseDesc MIPS.Program MIPS.Program
optMipsPhase = pure

prettyMipsPhase :: PhaseDesc MIPS.Program String
prettyMipsPhase = verboseLog "pretty-printing MIPS..."
              >*> pure . pretty
              <*< verboseLog "Done."

-- Notation: c = C, i = preprocessed, m = MIPS
cmPipe, imPipe :: String -> Pipe Compiler String String
cmPipe fname = cppPhase :|> parsePhase fname
               :|> optTacPhase :|> tac2MipsPhase
               :|> optMipsPhase :|> prettyMipsPhase :|> End

imPipe fname = parsePhase fname
               :|> optTacPhase :|> tac2MipsPhase
               :|> optMipsPhase :|> prettyMipsPhase :|> End

execPipe :: Monad m => Pipe m a b -> a -> m b
execPipe End = pure
execPipe (f :|> pipe) = f >=> execPipe pipe

pickPipe :: (Phase, Phase) -> String -> Pipe Compiler String String
pickPipe (CppPhase, PrettyMipsPhase)   = cmPipe
pickPipe (ParsePhase, PrettyMipsPhase) = imPipe
pickPipe (CppPhase, CppPhase)          = const (cppPhase :|> End)
