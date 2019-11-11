module Compiler.Pipeline where

import Options
import Pretty

import Data.Maybe (fromMaybe)

import System.IO
import System.Exit
import System.FilePath

import Compiler.Monad
import Compiler.Error

-- Compiler phases
import Parser.Parser (parseC)

import TAC.CodeGen (mipsCodeGenProc)

import qualified MIPS.Language as MIPS
import qualified MIPS.Pretty   as MIPS

import Control.Monad
import Control.Lens hiding ((<.>))


data Phase = CppPhase | CPhase | TacPhase | MipsPhase | OutPhase
  deriving (Eq, Ord, Show, Enum, Bounded)

compile :: Options -> FilePath -> IO ()
compile opts filename =
    let ext = takeExtension filename
    in case ext of
        ""   -> do putStrLn "file with no extension given - cspim does not contain a linker."
                   exitWith (ExitFailure 1)
        ".c" -> runPipeline opts CppPhase filename
        _    -> do putStrLn $ "file extension '" ++ ext ++ "' not recongized."
                   exitWith (ExitFailure 1)

runPipeline :: Options -> Phase -> FilePath -> IO ()
runPipeline opts phase fname = do
    source <- readFile fname
    let result = runCompiler (compileAction source) (opts^.flags)
        outFile = fromMaybe (dropExtension fname <.> "s") (opts^.outputFile)
    case result of
        This err    -> exitWithCompileError err
        That result -> writeFile outFile result
        These errs result -> do
            printCErrs errs
            writeFile outFile result

  where compileAction = finalizeStdErrOutput . pipeline (CppPhase, OutPhase) fname

exitWithCompileError :: [CErr] -> IO ()
exitWithCompileError errs = do
    printCErrs errs
    exitWith $ ExitFailure 1

--------------------------------------------------------------------------------------
-- The pipeline
--
-- This is still an early version; phase handling and compositionality is a future goal.
--------------------------------------------------------------------------------------

tempAdjustedCodeGenProc :: Show e => Either e [MIPS.MipsLine] -> Compiler [MIPS.MipsLine]
tempAdjustedCodeGenProc (Left e) =
    error $ "cspim internal temporary hack, file a bug! \
            \This error was produced and not handled:\n"
        ++ show e
tempAdjustedCodeGenProc (Right v) = return v

pipeline :: (Phase, Phase) -> FilePath -> String -> Compiler String
pipeline _ fname = (\str -> verboseLog "Starting parse" *> parseC fname str <* verboseLog "Done parsing")
               >=> tempAdjustedCodeGenProc . mipsCodeGenProc
               >=> return . MIPS.pretty
