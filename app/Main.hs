{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import VM.Emulator
import VM.Parser

import Compiler.Pipeline

import Control.Monad

import System.IO
import System.Exit
import System.Environment
import System.Console.CmdArgs.Implicit

main :: IO ()
main = do
    args <- getArgs
    when (null args) exitWithHelp
    args <- parseArgs
    case args of
        Compile { files = []  } -> withArgs ["compile", "-?"] parseArgs >> pure ()
        Compile { files = [f] } -> compile f
        Compile { files = _   } -> do
            putStrLn "Currently only supports one-file compilation."
            exitWith (ExitFailure 1)
        Emulate { file = Nothing } -> withArgs ["emulate", "-?"] parseArgs >> pure ()
        Emulate { file = Just f  } -> runEmulator f

data Modes
     = Compile { files :: [String] }
     | Emulate { file :: Maybe String }
  deriving (Show, Data, Typeable)

parseArgs :: IO Modes
parseArgs = cmdArgsRun mode

mode :: Mode (CmdArgs Modes)
mode = cmdArgsMode $ modes
    [ Emulate { file = def &= args &= typ "FILE" } &= help "Emulate a VM file"
    , Compile { files = def &= args &= typ "FILES" } &= help "Compile a group of files"
    ]
    &= program "cspim"
    &= summary "cspim v0.0.1 - Max Kopinsky & Ajay Tatachar (C) 2019"

runEmulator :: String -> IO ()
runEmulator fname = do
    contents <- readFile fname
    let parseResult = parseVM contents
    case parseResult of
        Left errs  -> mapM_ print errs
        Right prog -> emulate prog

exitWithHelp :: IO ()
exitWithHelp = do
    withArgs ["-?"] parseArgs
    exitSuccess
