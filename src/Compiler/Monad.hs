{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Compiler.Monad
    ( module Compiler.Monad.Class
    , module Compiler.Monad
    ) where

import Compiler.Monad.Class hiding (runCompilerMonad)
import qualified Compiler.Monad.Class as FMC

import Compiler.Flags
import Compiler.Error

import Data.List (partition)
import Data.DList
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Bifunctor

import Control.Monad.Reader
import Control.Monad.Validate
import qualified Control.Monad.Validate.Internal as V

-- | Converts all warnings in the current computation to errors.
--   Note that the monad-validate library makes it impossible to
--   write a function which /ignores/ all warnings in the
--   current computation. For that, see 'unwarnC', which requires
--   round-tripping.
werror :: MonadTrans t => t Compiler ()
werror = lift $ C $ lift $ V.validateT $ \case
    V.MNothing -> return $ Right (V.MNothing, ())
    V.MJust es -> return $ Left es

withTheseC :: FullMonadCompiler m => (These (DList CErr) a -> m b) -> m a -> m b
withTheseC f m = runMonadCompiler m >>= f
{-# INLINE withTheseC #-}

-- | Like withTheseC, but lets you give functions for each case rather than for the whole These.
--   A common idiom is presumed to be 'handleC' error warn ok where ...
handleC :: FullMonadCompiler m
        => (DList CErr -> m b)
        -> (DList CErr -> a -> m b)
        -> (a -> m b)
        -> m a
        -> m b
handleC this these that = withTheseC $ \case
    This e -> this e
    That a -> that a
    These e a -> these e a

-- | Takes a compiler action and causes any warnings it emits to become fatal errors.
werrorC :: FullMonadCompiler m => m a -> m a
werrorC = handleC error warn ok
  where error = compilerErrors
        warn e _ = compilerErrors e
        ok = return

-- | Takes a compiler action and ignores any warnings it emits.
unwarnC :: FullMonadCompiler m => m a -> m a
unwarnC = handleC error warn ok
  where error = compilerErrors
        warn _ a = return a
        ok = return

warnToErrorIf :: (forall e. CompileError e => e -> Bool) -> DList CErr -> Compiler ()
warnToErrorIf p ws = modifyCErrs2 errs warns $ toList ws
  where errs = const NoChange

        warns :: CompileError e => e -> CEAction -- type annotation is necessary
        warns e = if p e then W2Error else NoChange

modifyWarnings :: (forall e. CompileError e => e -> CEAction) -> [CErr] -> Compiler ()
modifyWarnings = modifyCErrs2 (const NoChange)

modifyErrors :: (forall e. CompileError e => e -> CEAction) -> [CErr] -> Compiler ()
modifyErrors act = modifyCErrs2 act (const NoChange)

-- | Given a list of 'CErrs', create a compiler action that throws them all.
--   Verbosity logs are always rethrown as verbosity logs, but warnings and errors
--   will be rethrown according to the action returned by the callback.
--
--   That is, 'Ignore' will /not/ rethrow the error, but 'NoChange' will.
modifyCErrs :: (forall e. CompileError e => e -> CEAction) -> [CErr] -> Compiler ()
modifyCErrs callback = modifyCErrs2 callback callback

-- | Like 'modifyCErrs', but takes separate callbacks for errors and warnings.
--
--   Can be used to distinguish between 'CErr's further than allowed by W2Error and E2Warning.
--   For example, you can 'Ignore' warnings, but send 'NoChange' for errors of the same type.
modifyCErrs2 :: (forall e. CompileError e => e -> CEAction) -- ^ with errors
             -> (forall e. CompileError e => e -> CEAction) -- ^ with warnings
             -> [CErr]
             -> Compiler ()
modifyCErrs2 pe pw = traverse_ adjustCErr
  where adjustCErr :: CErr -> Compiler ()
        adjustCErr v@(CErr VerboseLog _) = rethrowCErr v
        adjustCErr (CErr Warning w) = case pw w of
            W2Error -> compilerError w
            Ignore  -> return ()
            _       -> compilerWarning w
        adjustCErr (CErr Error e) = case pe e of
            E2Warning -> compilerWarning e
            Ignore    -> return ()
            _         -> compilerError e

finalizeStdErrOutput :: Compiler a -> Compiler a
finalizeStdErrOutput c = do
    flags <- compilerFlags
    if Verbose `isFlagSet` flags
    then c
    else handleC error warn ok c
  where error es  = compilerErrors  $ fromList $ removeVerboseLogs $ toList es
        warn ws a = compilerWarnings (fromList $ removeVerboseLogs $ toList ws) $> a
        ok = return
