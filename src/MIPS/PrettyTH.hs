{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module MIPS.PrettyTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Char
import Data.Maybe (fromJust)

import Control.Monad

prettyName :: Q Name
prettyName = fromJust <$> lookupTypeName "Pretty"

pprName :: Q Name
pprName = fromJust <$> lookupValueName "ppr"

miName :: Q Name
miName = fromJust <$> lookupTypeName "MipsInstruction"

makeRegPrettyInstances :: Q [Dec]
makeRegPrettyInstances = sequence [mkRegPrettyInstance "Reg", mkRegPrettyInstance "FReg"]

mkRegPrettyInstance :: String -> Q Dec
mkRegPrettyInstance typename = do
    reg <- fromJust <$> lookupTypeName typename
    ppr <- pprName
    TyConI (DataD _ _ _ _ ctors _) <- reify reg
    clauses <- mapM regPrettyClauseOfCtor ctors
    pretty <- prettyName
    return $ InstanceD Nothing [] (ConT pretty `AppT` ConT reg)
               [FunD ppr clauses]

regPrettyClauseOfCtor :: Con -> Q Clause
regPrettyClauseOfCtor (NormalC name btypes) = do
    let nonParsedName = '$' : map toLower (drop 3 $ nameBase name) -- drop "Reg" prefix
    body <- NormalB <$> [| text $(return $ LitE $ StringL nonParsedName) |]
    return $ Clause [ConP name []] body []

makeMIPrettyInstance :: Q [Dec]
makeMIPrettyInstance = do
    [pretty, mi, ppr] <- sequence [prettyName, miName, pprName]
    TyConI (DataD _ _ _ _ ctors _) <- reify mi
    clauses <- mapM miPrettyClauseOfCtor ctors
    return [InstanceD Nothing [] (ConT pretty `AppT` ConT mi) [FunD ppr clauses]]

miPrettyClauseOfCtor :: Con -> Q Clause
miPrettyClauseOfCtor (NormalC name btypes) = do
    let nonParsedName = mipsNameOf $ nameBase name
        numArgs = length btypes
    argNames <- replicateM numArgs (newName "x")
    let argPats = map VarP argNames
    ppr <- fromJust <$> lookupValueName "ppr"
    body <- NormalB <$> [| text $(liftString nonParsedName)
                           <> indent 15 (mconcat $ punctuate (text ", ")
                              $(return $ ListE $ map ((VarE ppr `AppE`) . VarE) argNames)) |]
    return $ Clause [ConP name argPats] body []

mipsNameOf :: String -> String
mipsNameOf ('M':name) = start name
  where start (c : rest) = toLower c : walk rest
        walk [] = []
        walk (c : rest) | isUpper c = '.' : toLower c : walk rest
                        | otherwise = c : walk rest
