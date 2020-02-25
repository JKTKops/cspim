{- |

This module handles the entire compiler pipeline. That includes constructing the compiler
phases and composing them according to the input file extension and stopping phase.

We execute cpp over the input file with 'unsafePerformIO'; see Compiler.Pipeline.CPP

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Compiler.Pipeline where
import Compiler.Pipeline.CPP

import Options
import Pretty

import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
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
import qualified MIPS.Peephole.Pass as MIPS
import MIPS.Pretty () -- pretty instances

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding ((<.>))
import Control.Lens.Utils

compile :: Options -> FilePath -> IO ()
compile opts filename =
    let ext = takeExtension filename
    in case ext of
        ""   -> do putStrLn "file with no extension given - don't know how to proceed."
                   exitWith (ExitFailure 1)
        ".c" -> runPipeline opts CppPhase filename
        ".i" -> runPipeline opts ParsePhase filename
        ".o" -> noLinker
        ".s" -> noLinker
        _    -> do putStrLn $ "file extension '" ++ ext ++ "' not recongized."
                   exitWith (ExitFailure 1)
  where
    noLinker = do putStrLn "cspim cannot link files (QtSpim only takes assembly)"
                  exitWith (ExitFailure 1)

runPipeline :: Options -> Phase -> FilePath -> IO ()
runPipeline opts startPhase fname = do
    result <- runCompilerIO compileAction (opts^.flags)
    let outFile = fromMaybe (rawFname <.> outputExt) (opts^.outputFile)
    writeDumps dumpFileName (getDumps result)
    -- TODO: handle the dumps!
    case getOutput result of
        This err    -> exitWithCompileError err
        That result -> T.writeFile outFile result
        These errs result -> do
            printCErrs errs
            T.writeFile outFile result

  where rawFname = dropExtension fname
        dumpFileName = if (opts^.flags) ? DumpToFile then Just rawFname else Nothing
        compileAction = finalizeStdErrOutput $ pipeline (startPhase, endPhase) fname

        -- This is abstraction overkill currently but may not be in the future.
        -- "Pick the earliest end phase which is enabled"
        endPhase = fromJust $ msum [cppEnd, Just PrettyMipsPhase]
          where cppEnd = if isFlagSet E theFlags then Just CppPhase else Nothing
                theFlags = opts^.flags

        outputExt = case endPhase of
            PrettyMipsPhase -> "s"
            CppPhase        -> "i"

writeDumps :: Maybe FilePath -> [Dump] -> IO ()
writeDumps fp dumps = forM_ dumps $ \(flag, text) -> do
    hdl <- handleFor flag
    T.hPutStrLn hdl text
  where handleFor flag = case fp of
            Nothing -> pure stderr
            Just fn -> openFile (fn <.> flagSkewerCase flag) WriteMode

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

pipeline :: (Phase, Phase) -> FilePath -> Compiler Text
pipeline phases fname = execPipe (pickPipe phases fname) fname

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

cppPhase :: PhaseDesc FilePath Text
cppPhase = pure . preprocess

parsePhase :: FilePath -> PhaseDesc Text TAC.Program
parsePhase fname src = do
    verboseLog "Start parsing phase..."
    prog <- parseC fname src
    verboseLog "End parsing phase."
    verboseLog "Checking if we should dump TAC..."
    ifM (isFlagSet DumpTac <$> compilerFlags) (dumpTac prog) (verboseLog "No.")
    return prog

  where dumpTac prog = do
            verboseLog "Yes..."
            dumpFlag DumpTac $ pretty prog
            verboseLog "Dumped TAC."

optTacPhase :: PhaseDesc TAC.Program TAC.Program
optTacPhase = pure -- TODO setup --dump-simpl (optimizer should dump while working)

tac2MipsPhase :: PhaseDesc TAC.Program MIPS.Program
tac2MipsPhase = verboseLog "Start instruction selection..."
            >*> mipsCodeGenProc
            <*< verboseLog "Done selecting instructions."

optMipsPhase :: PhaseDesc MIPS.Program MIPS.Program
optMipsPhase = verboseLog "Start MIPS optimizations..."
           >*> return . MIPS.runMipsPeepholePass
           <*< verboseLog "Done optimizing MIPS."

prettyMipsPhase :: PhaseDesc MIPS.Program Text
prettyMipsPhase = verboseLog "pretty-printing MIPS..."
              >*> pure . pretty
              <*< verboseLog "Done."

-- Notation: c = C, i = preprocessed, m = MIPS
cmPipe, imPipe :: FilePath -> Pipe Compiler FilePath Text
cmPipe fname = cppPhase :|> corePipe fname
imPipe = cmPipe -- cpp will nicely not touch files with .i extensions :)

corePipe :: FilePath -> Pipe Compiler Text Text
corePipe fname = parsePhase fname
           :|> optTacPhase :|> tac2MipsPhase
           :|> optMipsPhase :|> prettyMipsPhase :|> End

execPipe :: Monad m => Pipe m a b -> a -> m b
execPipe End = pure
execPipe (f :|> pipe) = f >=> execPipe pipe

pickPipe :: (Phase, Phase) -> FilePath -> Pipe Compiler FilePath Text
pickPipe (CppPhase, PrettyMipsPhase)   = cmPipe
pickPipe (ParsePhase, PrettyMipsPhase) = imPipe
pickPipe (CppPhase, CppPhase)          = const (cppPhase :|> End)
