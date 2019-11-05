{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Compiler.SymbolTable where

import TAC.Language
import qualified MIPS.Language as Mips

import Data.Int
import qualified Data.Map as M

import Control.Lens.TH
import Control.Monad.Reader

data SymbolTable = SymTab
    { _labelNames :: LabelMap String
    , _varNames   :: UniqueMap String
    , _varTypes   :: UniqueMap Type
    , _funcTable  :: UniqueMap Function
    , _allocTable :: UniqueMap MemLoc
    }

makeLenses ''SymbolTable

emptyTable :: SymbolTable
emptyTable = SymTab mapEmpty mapEmpty mapEmpty mapEmpty mapEmpty

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

--------------------------------------------------------------------------------------
--
-- IsMap instance for Data.Map to make life easier
--
--------------------------------------------------------------------------------------

instance Ord k => IsMap (M.Map k) where
    type KeyOf (M.Map k) = k
    mapNull              = M.null
    mapSize              = M.size
    mapMember            = M.member
    mapLookup            = M.lookup
    mapFindWithDefault   = M.findWithDefault
    mapEmpty             = M.empty
    mapSingleton         = M.singleton
    mapInsert            = M.insert
    mapInsertWith        = M.insertWith
    mapDelete            = M.delete
    mapUnion             = M.union
    mapUnionWithKey      = M.unionWithKey
    mapDifference        = M.difference
    mapIntersection      = M.intersection
    mapIsSubmapOf        = M.isSubmapOf
    mapMap               = M.map
    mapMapWithKey        = M.mapWithKey
    mapFold              = M.foldr
    mapFoldWithKey       = M.foldrWithKey
    mapFilter            = M.filter
    mapElems             = M.elems
    mapKeys              = M.keys
    mapToList            = M.toList
    mapFromList          = M.fromList
    mapFromListWith      = M.fromListWith
