module Compiler.Pipeline.CPP where

import System.Process
import qualified System.IO.Unsafe as Unsafe

import qualified Data.Text.Lazy as Lazy
import qualified Data.Text      as Strict

import Data.Text.Lazy.IO
import Prelude hiding (hGetContents, putStr, putStrLn)

import Data.Functor ((<&>))

-- TODO: no standard includes, include our own core library
cppProcess :: FilePath -> CreateProcess
cppProcess fname = (proc "cpp" [fname]) { std_out = CreatePipe }

preprocess :: FilePath -> Strict.Text
preprocess fname = Unsafe.unsafePerformIO $ do
    (_, Just stdout, _, _) <- createProcess $ cppProcess fname
    hGetContents stdout <&> Lazy.toStrict
