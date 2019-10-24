{-# LANGUAGE GADTs #-}
module TAC.Language
    ( module Compiler.Hoopl
    , Name, Constant, Var, Function(..), Insn(..), LValue(..), RValue(..), Binop(..)
    , Monop(..), Type(..)
    ) where

import Compiler.Hoopl

type Name = Unique
type Constant = Integer
type Var = Either Name Constant -- Uniques refer to variable names to disambiguate scope

data Function = Fn { name :: Name, args :: [Name], entry :: Label, body :: Graph Insn C C }

data Insn e x where
    Label      :: Label                     -> Insn C O
    Enter      :: Name                      -> Insn O O -- see note [Enter O/O]
    (:=)       :: LValue -> RValue          -> Insn O O
    Retrieve   :: Name                      -> Insn O O -- move (loc name) $v0
    Goto       :: Label                     -> Insn O C
    IfGoto     :: RValue -> Label  -> Label -> Insn O C -- cond iflabel elselable
    Call       :: Name -> [RValue] -> Label -> Insn O C -- func args return_label
    ReturnExp  :: Name -> RValue            -> Insn O C -- func expr
    ReturnVoid :: Name                      -> Insn O C -- func

{- NOTE [Enter O/O]
The enter f instruction is used to generate the prologue for function f.

Enter's shape is Open/Open because the entry label of the function it refers to is explicit
in the symbol table, but Enter isn't responsible for knowing it. Instead, the function's
assembly will begin with a real Label instruction, which is necessarily followed by an
Enter instruction.

If Enter were C/O, it would not be possible to write a NonLocal instance without adding
the label to its data.
-}

infixr 8 :=

data LValue
     = LVar Name
     | LIxArr Name Var -- x[i]

data RValue
     = RVar Var
     | RIxArr Name Var -- x[i]
     | Binop Var Binop Var
     | Monop Monop Var
     deriving (Eq, Ord, Show)

data Binop
     = Add
     | Sub
     | Mul
     | Div
     | Rem
     | ShiftR
     | ShiftL

     | Le
     | Lt
     | Ne
     | Eq
     | Gt
     | Ge
     deriving (Eq, Ord, Show, Enum, Bounded)

data Monop = Negate | Not deriving (Eq, Ord, Show, Enum, Bounded)

instance NonLocal Insn where
    entryLabel (Label l) = l

    successors (Goto l) = [l]
    successors (IfGoto _ t f) = [t, f]
    successors (Call _ _ r) = [r]
    successors (ReturnExp _ _) = []
    successors (ReturnVoid _) = []

data Type
     = CharTy
     | ShortTy
     | IntTy
     | UIntTy
     | FloatTy
     | DoubleTy
     | ArrType Int Type -- Type[Int]
