{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.Pipeline where

import qualified VM.Language   as VM
import qualified VM.Parser     as VM
import qualified VM.Translator as VM
import qualified MIPS.Language as MIPS
import qualified MIPS.Pretty   as MIPS

import System.IO
import System.Exit
import System.FilePath

import Control.Monad.Except

data CompileError
     = VmParseError [VM.ParseError]

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

newtype Compiler a = Compiler { unCompiler :: Except CompileError a }
  deriving (Functor, Applicative, Monad, MonadError CompileError)

data StartPhase = CppPhase | CPhase | VmPhase
  deriving (Eq, Ord, Show, Enum, Bounded)

compile :: FilePath -> IO ()
compile filename = do
    let ext = takeExtension filename
    case ext of
        "" -> do putStrLn "file with no extension given - cspim does not contain a linker."
                 exitWith (ExitFailure 1)
        ".vm" -> runPipeline VmPhase filename
        _ -> do putStrLn $ "file extension '" ++ ext ++ "' not recongized."
                exitWith (ExitFailure 1)

runPipeline :: StartPhase -> FilePath -> IO ()
runPipeline VmPhase fname = do
    source <- readFile fname
    let result = runExcept $ unCompiler $ translatePipelineSrcEntry source
    case result of
        Left err -> exitWithCompileError err
        Right result -> writeFile (dropExtension fname <.> "s") result

exitWithCompileError :: CompileError -> IO ()
exitWithCompileError (VmParseError errs) = do
    putStrLn "Errors while parsing VM file: "
    mapM_ print errs
    exitWith $ ExitFailure 1

--------------------------------------------------------------------------------------
-- The pipeline
--------------------------------------------------------------------------------------

translatePipelineSrcEntry :: String -> Compiler String
translatePipelineSrcEntry src = do
    prog <- liftEither $ mapLeft VmParseError $ VM.parseVM src
    translatePipeline prog

translatePipeline :: VM.VmProgram -> Compiler String
translatePipeline prog =
    let mips = VM.translate prog
    in return $ unlines $ map MIPS.pretty mips
