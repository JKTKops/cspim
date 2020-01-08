{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NondecreasingIndentation #-}
module MIPS.Peephole.PeepholeTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Mips.ParserTH -- has lots of utilities that I want

import Data.Maybe (fromJust)
import Data.Either (lefts)

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Extra

miName :: Q Name
miName = fromJust <$> lookupTypeName "MipsInstruction"

sourcesName, destsName :: Name
sourcesName = mkName "instSources"
destsName   = mkName "instDests"

mkSourcesAndDestsFuns :: Q [Dec]
mkSourcesAndDestsFuns = do
    TyConI (DataD _ _ _ _ ctors _) <- miName >>= reify
    (srcCls, destCls) <- collectSourcesAndDestsClauses ctors
    return [FunD sourcesName srcCls, FunD destsName destCls]

collectSourcesAndDestsClauses :: [Con] -> Q ([Clause], [Clause])
collectSourcesAndDestsClauses = mapAndUnzipM (mkSourcesAndDestsClauses . nameOfCon)
  where nameOfCon (NormalC name _) = name

mkSourcesAndDestsClauses :: Name -> Q (Clause, Clause)
mkSourcesAndDestsClauses con = do
    miName <- miName
    DataConI _ type_info parentName <- reify con
    when (miName /= parentName) $
        fail $ "Data constructor " ++ show con
               ++ " does not belong to type MipsInstruction"
    let arg_types = unpackFunType type_info
    (srcPats, srcs)   <- mkSrcPats arg_types
    (destPats, dests) <- mkDestPats arg_types
    sourceBody <- NormalB <$> [| lefts $(pure $ ListE srcs) |]
    let sourceClause = Clause [ConP con srcPats] sourceBody []
        destsBody = NormalB $ ListE $ map VarE dests
        destsClause = Clause [ConP con destPats] destsBody []
    return (sourceClause, destsClause)

mkSrcPats :: [Name] -> Q ([Pat], [Exp])
mkSrcPats names = (reverse *** reverse) <$> foldM createPat ([], []) names
  where
    createPat state name =
        ifM ((== name) <$> rsrcName)
            (addSrcPat state) $
        -- else
        ifM ((== name) <$> src2Name)
            (addSrc2Pat state) $
        -- else
        addWildPat state

    addSrcPat (pats, exps) = do
        name <- newName "src"
        return (VarP name : pats, AppE (ConE 'Left) (VarE name) : exps)
    addSrc2Pat (pats, exps) = do
        name <- newName "src"
        return (VarP name : pats, VarE name : exps)
    addWildPat (pats, names) = return (WildP : pats, names)

mkDestPats :: [Name] -> Q ([Pat], [Name])
mkDestPats names = (reverse *** reverse) <$> foldM createPat ([], []) names
  where
    createPat state name =
        ifM ((== name) <$> rdestName)
            (addDestPat state) $
        -- else
        addWildPat state

    addDestPat (pats, names) = do
        name <- newName "dest"
        return (VarP name : pats, name : names)
    addWildPat (pats, names) = return (WildP : pats, names)
