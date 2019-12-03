{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Compiler.Monad.Class
    ( -- * Responses from compiler monads
      These(..)

    -- * The Compiler monad and the MonadCompiler classes
    , Compiler(..), runCompiler, runCompilerIO, testCompilerIO

    , MonadCompiler(verboseLog, compilerWarnings, compilerErrors, compilerFlags)
    , FullMonadCompiler(..)

    -- * Nice functions for emitting singleton errors/warnings
    , compilerError
    , compilerWarning
    , rethrowCErr

    -- * Throw any string as an error, generally indicating an invariant violation.
    , panic

    ) where

import Pretty
import Compiler.Error
import Compiler.Flags
import Compiler.Monad.UniqueSupply
import qualified Compiler.Hoopl as Hoopl
import Data.DList

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Bifunctor

import Control.Monad.Validate
import Control.Monad.Identity

import Control.Monad.Reader
import Control.Monad.Except
import qualified Control.Monad.Writer.Lazy    as Lazy
import qualified Control.Monad.Writer.Strict  as Strict
import qualified Control.Monad.State.Lazy     as Lazy
import qualified Control.Monad.State.Strict   as Strict
import qualified Control.Monad.RWS.Lazy       as Lazy
import qualified Control.Monad.RWS.Strict     as Strict
import Text.Parsec (ParsecT)

import qualified Control.Monad.Validate.Internal as V

import Control.Monad.Debug

-- | The classic (E + A + EA) data type. Has the obvious functor instance,
--   But /not/ the applicative/monad instances. Use 'Validate' instead.
data These e a = This e | That a | These e a
  deriving (Eq, Ord, Show)

-- | The class representing monads in the compiler tool stack.
--   Compiler monads must be able to make verbosity logs, raise warnings and errors,
--   and additionally must be able to to ask for the current compilation flags.
class Monad m => MonadCompiler m where
    -- | Log a string to the verbosity log.
    verboseLog :: Text -> m ()

    -- | Raise a list of compiler warnings. If the input list is empty, the
    --   compiler's state should not be affected.
    compilerWarnings :: DList CErr -> m ()

    -- | Throw a list of compiler errors. If the input list is empty, the result should be
    --   @m 'undefined'@, but the compiler should /not/ enter an error state.
    --   Law: @m <* compilerErrors [] == m@ and @compilerErrors [] *> m == m@.
    compilerErrors   :: DList CErr -> m a
    compilerFlags    :: m Flags

    freshLabel  :: m Hoopl.Label
    freshUnique :: m Hoopl.Unique


instance {-# OVERLAPPABLE #-} (Monad m, MonadCompiler m) => Hoopl.UniqueMonad m where
    freshUnique = Compiler.Monad.Class.freshUnique

-- | Raise a single warning.
compilerWarning :: (MonadCompiler m, CompileError e) => e -> m ()
compilerWarning = compilerWarnings . singleton . CErr Warning

-- | Throw a single error.
compilerError :: (MonadCompiler m, CompileError e) => e -> m a
compilerError = compilerErrors . singleton . CErr Error

-- | Overloaded panic function for any 'MonadCompiler'.
panic :: (MonadCompiler m) => Text -> m a
panic = compilerError . Panic

rethrowCErr :: (MonadCompiler m) => CErr -> m ()
rethrowCErr (CErr VerboseLog e) = verboseLog $ pretty e
rethrowCErr (CErr Warning w) = compilerWarning w
rethrowCErr (CErr Error e) = compilerError e

-- | Some monads in the compiler stack are additionally capable of running the
--   underlying compiler monad to obtain a (monadic) representation of the current
--   error\/warn\/ok state.
--
--   In fact, it may be that any MonadCompiler is also a FullMonadCompiler,
--   but it is at least the case that ParsecT does not provide the mapParsecT
--   function and it's not entirely clear that it is possible in its abscence.
class MonadCompiler m => FullMonadCompiler m where
    runMonadCompiler :: m a -> m (These (DList CErr) a)

-- | The monadic type of compilation actions. Compilation actions have access to the
--   compilation flags, can raise warnings, and can throw errors.
newtype Compiler a = C { unC :: ReaderT Flags (ValidateT (DList CErr) UniqueSM) a }
  deriving (Functor, Applicative, Monad)

-- | Run a compilation action with the given flags, returning either:
--
--   'This' errs    - a list of compilation errors, possibly also containing warnings
--
--   'These' errs a - the result of the action, and a (nonempty-)list of compilation warnings
--
--   'That' a       - the result of the action, which completed with no errors or warnings.
runCompiler :: Compiler a -> UniqueSupply -> Flags -> These [CErr] a
runCompiler (C m) us flags = runReaderT m flags & evalValidateT & evalUniqueSM us & first toList

-- | Run a compiler in the IO monad, using the standard unique supply.
runCompilerIO :: Compiler a -> Flags -> IO (These [CErr] a)
runCompilerIO c flags = do
    us <- mkUniqueSupply
    return $ runCompiler c us flags

-- | Use this in functions that need to test run compilers.
--   It resets the gensym to counter 0 and inc 1, then calls runCompilerIO.
testCompilerIO :: Compiler a -> Flags -> IO (These [CErr] a)
testCompilerIO c flags = do
    initGenSym 0 1
    runCompilerIO c flags


instance MonadCompiler Compiler where
    verboseLog s = C $ dispute $ singleton $ CErr VerboseLog (VL s)
    compilerWarnings l@(DL f) | (_:_) <- f [] = C $ dispute $ fmap toWarning l
                              | otherwise     = return ()
      where toWarning (CErr _ e) = CErr Warning e
    compilerErrors l@(DL f)   | (_:_) <- f [] = C $ refute  $ fmap toError l
                              | otherwise     = return undefined
      where toError (CErr _ e) = CErr Error e
    compilerFlags    = C ask

    freshUnique = C $ lift $ lift Hoopl.freshUnique
    freshLabel = C $ lift $ lift Hoopl.freshLabel

instance FullMonadCompiler Compiler where
    runMonadCompiler (C m) = C $ do
        us0   <- lift $ lift unsafeGetUniqueSupplyM
        flags <- ask
        return $ runReaderT m flags & evalValidateT & evalUniqueSM us0

instance MonadCompiler m => MonadCompiler (ReaderT r m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance FullMonadCompiler m => FullMonadCompiler (ReaderT r m) where
    runMonadCompiler = mapReaderT runMonadCompiler

instance MonadCompiler m => MonadCompiler (ExceptT e m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance FullMonadCompiler m => FullMonadCompiler (ExceptT e m) where
    runMonadCompiler = mapExceptT $ \this -> this >>= \case
        Left e  -> return $ Left e
        Right a -> Right <$> runMonadCompiler (return a)

instance (MonadCompiler m, Monoid w) => MonadCompiler (Lazy.WriterT w m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance (FullMonadCompiler m, Monoid w) => FullMonadCompiler (Lazy.WriterT w m) where
    runMonadCompiler = Lazy.mapWriterT $ \this -> do
        ~(a, w) <- this
        these   <- runMonadCompiler (return a)
        return (these, w)

instance (MonadCompiler m, Monoid w) => MonadCompiler (Strict.WriterT w m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance (FullMonadCompiler m, Monoid w) => FullMonadCompiler (Strict.WriterT w m) where
    runMonadCompiler = Strict.mapWriterT $ \this -> do
        (a, w) <- this
        these  <- runMonadCompiler (return a)
        return (these, w)

instance MonadCompiler m => MonadCompiler (Lazy.StateT s m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance FullMonadCompiler m => FullMonadCompiler (Lazy.StateT s m) where
    runMonadCompiler = Lazy.mapStateT $ \this -> do
        ~(a, s) <- this
        these   <- runMonadCompiler (return a)
        return (these, s)

instance MonadCompiler m => MonadCompiler (Strict.StateT s m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance FullMonadCompiler m => FullMonadCompiler (Strict.StateT s m) where
    runMonadCompiler = Strict.mapStateT $ \this -> do
        (a, s) <- this
        these  <- runMonadCompiler (return a)
        return (these, s)

instance (MonadCompiler m, Monoid w) => MonadCompiler (Lazy.RWST r w s m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance (FullMonadCompiler m, Monoid w) => FullMonadCompiler (Lazy.RWST r w s m) where
    runMonadCompiler = Lazy.mapRWST $ \this -> do
        ~(a, s, w) <- this
        these      <- runMonadCompiler (return a)
        return (these, s, w)

instance (MonadCompiler m, Monoid w) => MonadCompiler (Strict.RWST r w s m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

instance (FullMonadCompiler m, Monoid w) => FullMonadCompiler (Strict.RWST r w s m) where
    runMonadCompiler = Strict.mapRWST $ \this -> do
        (a, s, w) <- this
        these     <- runMonadCompiler (return a)
        return (these, s, w)

instance MonadCompiler m => MonadCompiler (ParsecT s u m) where
    verboseLog       = lift . verboseLog
    compilerWarnings = lift . compilerWarnings
    compilerErrors   = lift . compilerErrors
    compilerFlags    = lift compilerFlags
    freshLabel       = lift freshLabel
    freshUnique      = lift freshUnique

evalValidateT :: Functor m => ValidateT e m a -> m (These e a)
evalValidateT m = V.unValidateT V.MNothing m <&> \case
  Left e                -> This e
  Right (V.MJust e, v)  -> These e v
  Right (V.MNothing, v) -> That v

evalValidate :: Validate e a -> These e a
evalValidate = runIdentity.evalValidateT

--------------------------------------------------------------------------------------
--
-- Some instances for 'These'
--
--------------------------------------------------------------------------------------

instance Functor (These e) where
    fmap _ (This e)    = This e
    fmap f (That a)    = That (f a)
    fmap f (These e a) = These e (f a)

instance Bifunctor These where
    bimap f g (This e)    = This (f e)
    bimap f g (That a)    = That (g a)
    bimap f g (These e a) = These (f e) (g a)
    {-# INLINE bimap #-}
    -- the default definition of first and second
    -- is already optimal when bimap is inlined.
