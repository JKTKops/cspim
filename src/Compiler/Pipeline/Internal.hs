module Compiler.Pipeline.Internal where

import System.IO.Unsafe

type Dump = (FilePath, String)

dump :: Applicative f => Dump -> f ()
dump (fname, contents) = unsafePerformIO $ do
    writeFile fname contents
    return $ pure ()
{-# NOINLINE dump #-}
