{-# LANGUAGE DeriveDataTypeable #-}
module Main where

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

newtype Modes
     = Compile { files :: [String] }
  deriving (Show, Data, Typeable)

parseArgs :: IO Modes
parseArgs = cmdArgsRun mode

mode :: Mode (CmdArgs Modes)
mode = cmdArgsMode $ modes
    [ Compile { files = def &= args &= typ "FILES" } &= help "Compile a group of files"
    ]
    &= program "cspim"
    &= summary "cspim v0.0.1 - Max Kopinsky & Ajay Tatachar (C) 2019"

exitWithHelp :: IO ()
exitWithHelp = do
    withArgs ["-?"] parseArgs
    exitSuccess
