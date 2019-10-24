{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module TAC.CodeGen where

import GHC.Exts (IsList (..))

import TAC.Language
import TAC.SymbolTable
import MIPS.Language
import MIPS.Parser (mips)

import Control.Monad.Except
import Control.Monad.RWS.Strict

newtype DiffList a = DL { unDL :: [a] -> [a] }

instance IsList (DiffList a) where
    type Item (DiffList a) = a
    fromList l = DL $ \tail -> l ++ tail
    toList (DL f) = f []

instance Semigroup (DiffList a) where
    DL f <> DL g = DL (f . g)

instance Monoid (DiffList a) where
    mappend = (<>)
    mempty  = DL id

data CGError = UniqueNotInMap Unique

instance Show CGError where
    show (UniqueNotInMap u) = "Unique " ++ show u ++ " could not be found in symbol table."

newtype CodeGen a = CG { unCG :: ExceptT CGError (RWS SymbolTable (DiffList MipsLine) ()) a }
  deriving ( Functor, Applicative, Monad, MonadError CGError
           , MonadReader SymbolTable, MonadWriter (DiffList MipsLine))

insnCodeGen :: Insn e x -> CodeGen ()
insnCodeGen (Label l) = labelCodeGen l
insnCodeGen (Enter f) = enterCodeGen f
insnCodeGen (lvalue := rvalue) = assignCodeGen lvalue rvalue
insnCodeGen (Retrieve var) = retrieveCodeGen var
insnCodeGen (Goto l) = gotoCodeGen l
insnCodeGen (IfGoto rvalue t f) = ifgotoCodeGen rvalue t f
insnCodeGen (Call f args ret_label) = callCodeGen f args -- drop ret_label as only Hoopl cares
insnCodeGen (ReturnExp f exp) = returnECodeGen f exp
insnCodeGen (ReturnVoid f) = returnVCodeGen f

undefined !!
