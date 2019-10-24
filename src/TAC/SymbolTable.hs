module TAC.SymbolTable where

import TAC.Language
import MIPS.Language

import Data.Word

data MemLoc = OffsetLoc Word32 | RegLoc Reg | FRegLoc FReg

data SymbolTable = SymTab
    { _labelNames :: LabelMap String
    , _varNames   :: UniqueMap String
    , _varTypes   :: UniqueMap Type
    , _funcTable  :: UniqueMap Function
    , _allocTable :: UniqueMap MemLoc
    }
