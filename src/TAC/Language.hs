{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module TAC.Language
    ( module Compiler.Hoopl
    , module TAC.Language
    ) where

import Compiler.Hoopl
import Control.Lens.TH

type Name = Unique

data Constant
     = IntConst    Integer
     | FloatConst  Double
     | StringConst String
     deriving (Eq, Ord, Show)

intValueOfConst :: Integral a => Constant -> a
intValueOfConst (IntConst i) = fromIntegral i
intValueOfConst (FloatConst d) = truncate d

type Var = Either Name Constant -- Uniques refer to variable names to disambiguate scope

data Insn e x where
    Label      :: Label                     -> Insn C O
    Enter      :: Name                      -> Insn O O -- see note [Enter O/O]
    (:=)       :: LValue -> RValue          -> Insn O O
    Retrieve   :: Name                      -> Insn O O -- move (loc name) $v0
    Goto       :: Label                     -> Insn O C
    IfGoto     :: RValue -> Label  -> Label -> Insn O C -- cond iflabel elselable
    Call       :: Name -> [RValue] -> Label -> Insn O C -- func args return_label
    Return     :: Name -> Maybe RValue      -> Insn O C -- func (expr or void)

instance Show (Insn e x) where
    show (Label lbl) = show lbl ++ ":"
    show (Enter f)   = "enter " ++ show f
    show (lvalue := rvalue) = show lvalue ++ " := " ++ show rvalue
    show (Retrieve x) = "retrieve " ++ show x
    show (Goto lbl)   = "goto " ++ show lbl
    show (IfGoto rv tl fl) = "ifgoto (" ++ show rv ++") " ++ show tl ++ " " ++ show fl
    show (Call f args _) = unwords ["call", show f, show args]
    show (Return _ rv) = "return" ++ maybe "" ((" " ++) . show) rv

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
     deriving (Eq, Ord, Enum, Bounded)

data Monop = Negate | Not deriving (Eq, Ord, Enum, Bounded)

instance Show LValue where
    show (LVar name) = show name
    show (LIxArr name ix) = show name ++ "[" ++ show ix ++ "]"

instance Show RValue where
    show (RVar var) = show var
    show (RIxArr name ix) = show name ++ "[" ++ show ix ++ "]"
    show (Binop lv op rv) = unwords [show lv, show op, show rv]
    show (Monop op v) = show op ++ show v

instance Show Binop where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Rem = "%"
    show ShiftR = ">>"
    show ShiftL = "<<"
    show Le = "<="
    show Lt = "<"
    show Ne = "!="
    show Eq = "=="
    show Gt = ">"
    show Ge = ">="

instance Show Monop where
    show Negate = "-"
    show Not    = "!"

instance NonLocal Insn where
    entryLabel (Label l) = l

    successors (Goto l) = [l]
    successors (IfGoto _ t f) = [f, t] -- this makes postorder_dfs give us true-fallthrough
    successors (Call _ _ r) = [r]
    successors (Return _ _) = []

instance HooplNode Insn where
    mkBranchNode = Goto
    mkLabelNode = Label

data Type
     = CharTy Signage
     | ShortTy Signage
     | IntTy Signage
     | FloatTy
     | DoubleTy
     | ArrTy Int Type -- Type[Int]
     deriving (Eq, Ord, Show)

data Signage = Signed | Unsigned
  deriving (Eq, Ord, Show, Enum, Bounded)

isIntTy :: Type -> Bool
isIntTy (IntTy _) = True
isIntTy _         = False

sizeof :: Type -> Int
sizeof (CharTy _)  = 1
sizeof (ShortTy _) = 2
sizeof (IntTy _)   = 4
sizeof FloatTy     = 4
sizeof DoubleTy    = 8
sizeof (ArrTy n t) = n * sizeof t

sllAlignment :: Type -> Maybe Int
sllAlignment ty = let s = sizeof ty
                      t = round $ logBase 2 $ fromIntegral s in
    if s `mod` (2 ^ t) == 0
    then Just t else Nothing

data TacGraph = TacGraph
    { _graph :: Graph Insn C C
    , _entry :: Label
    }
makeLenses ''TacGraph

type TacBlock = Block Insn C C

data Function = Fn
     { _name   :: Name
     , _args   :: [Name]
     , _locals :: [Name] -- this includes temporary variables generated during tacifier
     , _body   :: TacGraph
     }

makeLenses ''Function
