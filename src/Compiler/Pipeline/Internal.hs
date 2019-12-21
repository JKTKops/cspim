{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Compiler.Pipeline.Internal where

import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Dump = (FilePath, T.Text)

dump :: Applicative f => Dump -> f ()
dump (fname, contents) = unsafePerformIO $ do
    T.writeFile fname contents
    return $ pure ()
{-# NOINLINE dump #-}
