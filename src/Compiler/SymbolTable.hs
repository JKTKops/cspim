{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Compiler.SymbolTable where

import TAC.Language
import qualified MIPS.Language as Mips

import Data.Int

import Control.Lens.TH
import Control.Monad.Reader

data MemLoc = OffsetLoc Int32 | RegLoc Mips.Reg | FRegLoc Mips.FReg

data SymbolTable = SymTab
    { _labelNames :: LabelMap String
    , _varNames   :: UniqueMap String
    , _varTypes   :: UniqueMap Type
    , _funcTable  :: UniqueMap Function
    , _allocTable :: UniqueMap MemLoc
    }

makeLenses ''SymbolTable

lookupLabelName :: SymbolTable -> Label -> Maybe String
lookupLabelName symtab lbl = let lmap = _labelNames symtab
                             in mapLookup lbl lmap

lookupVarName :: SymbolTable -> Unique -> Maybe String
lookupVarName symtab uniq = let vmap = _varNames symtab
                            in mapLookup uniq vmap

lookupVarType :: SymbolTable -> Unique -> Maybe Type
lookupVarType st uniq = let tmap = _varTypes st in mapLookup uniq tmap

lookupFuncTable :: SymbolTable -> Unique -> Maybe Function
lookupFuncTable st uniq = mapLookup uniq (_funcTable st)

lookupMemLoc :: SymbolTable -> Unique -> Maybe MemLoc
lookupMemLoc st uniq = mapLookup uniq (_allocTable st)

askSymTabM :: MonadReader SymbolTable m
           => (SymbolTable -> Unique -> Maybe a)
           -> Unique
           -> m (Maybe a)
askSymTabM req = (asks req Prelude.<*>) . pure


type AskSymTabM k v = forall m. MonadReader SymbolTable m => k -> m (Maybe v)

askLabelNameM :: AskSymTabM Label String
askVarNameM   :: AskSymTabM Unique String
askVarTypeM   :: AskSymTabM Unique Type
askFuncTableM :: AskSymTabM Unique Function
askMemLocM    :: AskSymTabM Unique MemLoc

askLabelNameM lbl = asks lookupLabelName Prelude.<*> pure lbl
askVarNameM       = askSymTabM lookupVarName
askVarTypeM       = askSymTabM lookupVarType
askFuncTableM     = askSymTabM lookupFuncTable
askMemLocM        = askSymTabM lookupMemLoc
