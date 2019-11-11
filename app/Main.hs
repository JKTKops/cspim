module Main where

import Options
import Compiler.Pipeline

import System.IO
import System.Exit
import System.Environment

main :: IO ()
main = do
    opts <- parseArgs
    case opts of
        Options { _inputFiles = []  } ->
            putStrLn "cspim: no input files." >> exitWith (ExitFailure 1)
        Options { _inputFiles = [f] } -> compile opts f
        Options { _inputFiles = _   } -> do
            putStrLn "cspim currently only supports one-file compilation."
            exitWith (ExitFailure 1)
