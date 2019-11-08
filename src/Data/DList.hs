{- |
I'm not using the DList library because it's heavier than what I need.
I'd rather not include and extra dependency on it and risk accidentally
using one of its (unfortunately many) functions which force the DList
and eliminate the constant-time appending gains. DLists should only be
used inside CSpim in places where the the /only/ operation being performed
on the list is appending, and then it is immediately transformed into a
regular Haskell list.

See: TAC.CodeGen, Compiler.Monad
-}
{-# LANGUAGE TypeFamilies #-}
module Data.DList where

import qualified GHC.Exts as E (IsList(..))

newtype DList a = DL { unDL :: [a] -> [a] }

instance E.IsList (DList a) where
    type Item (DList a) = a
    fromList l = DL $ \tail -> l ++ tail
    toList (DL f) = f []

fromList :: [a] -> DList a
fromList = E.fromList

toList :: DList a -> [a]
toList = E.toList

singleton :: a -> DList a
singleton x = DL $ \tl -> x : tl
{-# INLINE singleton #-}

cons :: a -> DList a -> DList a
cons x (DL f) = DL $ (x:) . f
{-# INLINE cons #-}


snoc :: DList a -> a -> DList a
snoc (DL f) x = DL $ f . (x:)
{-# INLINE snoc #-}


instance Semigroup (DList a) where
    DL f <> DL g = DL (f . g)
    {-# INLINE (<>) #-}


instance Monoid (DList a) where
    mappend = (<>)
    mempty  = DL id

instance Show a => Show (DList a) where
    show (DL xs ) = "DL " ++ show (xs [])

instance Functor DList where
    fmap f (DL dl) = DL $ \tl -> map f (dl []) ++ tl
