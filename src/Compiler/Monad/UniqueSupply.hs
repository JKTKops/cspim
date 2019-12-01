-- | This module and the corresponding GenSym.c file heavily inspired by
--   GHC's files of analogous names; see ghc/compiler/basicTypes/UniqSupply.hs
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
module Compiler.Monad.UniqueSupply where

import Compiler.Hoopl (UniqueMonad(..), Unique)
import System.IO.Unsafe

-- | Generate a unique. Different every time genSym is called, at least between initGenSym calls
foreign import ccall unsafe "GenSym.c genSym" genSym :: IO Int
-- | Initialize the unique generator with a counter and step.
foreign import ccall unsafe "GenSym.c initGenSym" initGenSym :: Int -> Int -> IO ()

data UniqueSupply = MkSplitUniqueSupply
                  {-# UNPACK #-} !Int
                  UniqueSupply UniqueSupply

mkUniqueSupply :: IO UniqueSupply
mkUniqueSupply = let mk_supply = unsafeInterleaveIO $
                         MkSplitUniqueSupply <$> genSym <*> mk_supply <*> mk_supply
                 in mk_supply

-- | Get the initial unique supply. Care must be taken, as always, to use it only once.
initUniqueSupply :: UniqueSupply
initUniqueSupply = unsafePerformIO mkUniqueSupply
{-# NOINLINE initUniqueSupply #-}

splitUniqueSupply :: UniqueSupply -> (UniqueSupply, UniqueSupply)
splitUniqueSupply (MkSplitUniqueSupply _ s1 s2) = (s1, s2)

listSplitUniqueSupply :: UniqueSupply -> [UniqueSupply]
listSplitUniqueSupply (MkSplitUniqueSupply _ s1 s2) = s1 : listSplitUniqueSupply s2

uniqueFromSupply :: UniqueSupply -> Unique
uniqueFromSupply (MkSplitUniqueSupply i _ _) = i

-- | Get an infinite list of uniques from a UniqueSupply.
--   Ghc passes the right hand side to the recursive call, so I do too, but I don't know
--   why this is better.
uniquesFromSupply :: UniqueSupply -> [Unique] -- infinite
uniquesFromSupply (MkSplitUniqueSupply i _ s2) = i : uniquesFromSupply s2


takeUniqueFromSupply :: UniqueSupply -> (Unique, UniqueSupply)
takeUniqueFromSupply (MkSplitUniqueSupply i s1 _) = (i, s1)

--------------------------------------------------------------------------------------
--
-- Fast state monad transformer for generating uniques
--
--------------------------------------------------------------------------------------

#if !defined(LOADED_INTO_GHCI)

pattern UniqueResult :: a -> b -> (# a, b #)
pattern UniqueResult r us = (# r, us #)

{-# COMPLETE UniqueResult #-}

type UniqueResult result = (# result, UniqueSupply #)

#else

data UniqueResult result = UniqueResult !result {-# UNPACK #-} !UniqueSupply

#endif

newtype UniqueSM result = USM { unUSM :: UniqueSupply -> UniqueResult result }

instance Functor UniqueSM where
    fmap f (USM usf) = USM $ \us0 ->
        case usf us0 of
            UniqueResult r us1 -> UniqueResult (f r) us1

instance Applicative UniqueSM where
    pure x = USM $ \us0 -> UniqueResult x us0
    (USM f) <*> (USM x) = USM $ \us0 -> case f us0 of
        UniqueResult ff us1 -> case x us1 of
            UniqueResult xx us2  -> UniqueResult (ff xx) us2

instance Monad UniqueSM where
    return = pure
    (USM a) >>= fab = USM $ \us0 -> case a us0 of
        UniqueResult aa us1 -> unUSM (fab aa) us1

runUniqueSM :: UniqueSupply -> UniqueSM a -> (a, UniqueSupply)
runUniqueSM us m = case unUSM m us of
    UniqueResult r us -> (r, us)

evalUniqueSM :: UniqueSupply -> UniqueSM a -> a
evalUniqueSM us m = case unUSM m us of
    UniqueResult r _ -> r

getUniqueSupplyM :: UniqueSM UniqueSupply
getUniqueSupplyM = USM $ \us0 -> case splitUniqueSupply us0 of
    (us1, us2) -> UniqueResult us1 us2

instance UniqueMonad UniqueSM where
    freshUnique = USM $ \us0 -> case takeUniqueFromSupply us0 of
        (u, us1) -> UniqueResult u us1
