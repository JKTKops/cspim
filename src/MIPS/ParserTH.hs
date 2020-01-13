{-
Rather than write down all however-many-hundred-cases of mips instructions by hand
I'm going to write some TemplateHaskell to do it for me!
-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TemplateHaskell #-}
module MIPS.ParserTH where

import MIPS.Language

import Language.Haskell.TH hiding (bang, bangType)
import Language.Haskell.TH.Syntax

import Text.Parsec ((<|>))

import Data.Char
import Data.Data

import Control.Monad

qtname :: String -> Q Name
qtname name = do
    Just n <- lookupTypeName name
    return n

qvname :: String -> Q Name
qvname name = do
    Just n <- lookupValueName name
    return n

mipsInstructionName :: Q Name
mipsInstructionName = qtname "MipsInstruction"

labelName :: Q Name
labelName = qtname "Label"

rdestName, rsrcName, src2Name, frdestName, frsrcName, immName, addressName :: Q Name
rdestName   = qtname "RDest"
rsrcName    = qtname "RSrc"
src2Name    = qtname "Src2"
frdestName  = qtname "FRDest"
frsrcName   = qtname "FRSrc"
immName     = qtname "Imm"
addressName = qtname "Address"

bang :: Bang
bang = Bang NoSourceUnpackedness NoSourceStrictness

bangType :: Type -> BangType
bangType t = (bang, t)

makeParsedTypeDefinitions :: Q [Dec]
makeParsedTypeDefinitions = sequence [mkParsedMIType, mkParsedMDType]

mkParsedMIType, mkParsedMDType :: Q Dec
mkParsedMIType = do
    min <- mipsInstructionName
    TyConI (DataD _ _ _ _ ctors _) <- reify min
    let typename = mkName "ParsedMipsInstruction"
    ctors' <- createParsedCtors ctors
    return $ DataD [] typename [] Nothing ctors' derivs

mkParsedMDType =
    let typename = mkName "ParsedMipsDeclaration"
        directive = NormalC (mkName "PMDirective") [bangType $ ConT $ mkName "Directive"]
        label = NormalC (mkName "PMLabel") [bangType $ ConT $ mkName "ParsedLabel"]
        inst = NormalC (mkName "PMInst") [bangType $ ConT $ mkName "ParsedMipsInstruction"]
    in return $ DataD [] typename [] Nothing [directive, label, inst] derivs

createParsedCtors :: [Con] -> Q [Con]
createParsedCtors = mapM createParsedCtor

createParsedCtor :: Con -> Q Con
createParsedCtor (NormalC th_name th_args) = do
    let ctor_name = mkName $ 'P' : nameBase th_name
    ctor_args <- mapM mkParsedArg th_args
    return $ NormalC ctor_name ctor_args

mkParsedArg :: BangType -> Q BangType
mkParsedArg bt@(b, ConT t) = do
    specials <- sequence [ immName, src2Name, labelName, addressName
                         , rdestName, rsrcName, frdestName, frsrcName ]

    if t `elem` specials
    then return (b, ConT (mkName $ "Parsed" ++ nameBase t))
    else return bt

derivs :: [DerivClause]
derivs = [DerivClause Nothing [ConT ''Typeable, ConT ''Data]]

makePMILift :: Q [Dec]
makePMILift = do
    TyConI (DataD _ _ _ _ ctors _) <- reify $ mkName "ParsedMipsInstruction"
    clauses <- mapM liftClauseOfCtor ctors
    let pmiInstance = InstanceD
                        Nothing
                        []
                        (ConT ''Lift `AppT` ConT (mkName "ParsedMipsInstruction"))
                        [FunD (mkName "lift") clauses]
    TyConI (DataD _ _ _ _ ctors' _) <- reify $ mkName "ParsedMipsDeclaration"
    clauses' <- mapM liftClauseOfCtor ctors'
    let pmdInstance = InstanceD
                        Nothing
                        []
                        (ConT ''Lift `AppT` ConT (mkName "ParsedMipsDeclaration"))
                        [FunD (mkName "lift") clauses']
    return [pmiInstance, pmdInstance]

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

liftClauseOfCtor :: Con -> Q Clause
liftClauseOfCtor (NormalC name btypes) = do
    let nonParsedName = tail $ nameBase name
        numArgs = length btypes
    argNames <- replicateM numArgs (newName "x")
    let argPats = map VarP argNames
    body <- NormalB <$> [| $(varE 'apply) (mkName $(return $ LitE $ StringL nonParsedName))
                             $(return $ ListE $ map ((VarE 'lift `AppE`) . VarE) argNames) |]
    return $ Clause [ConP name argPats] body []

labelParserName :: Q Name
labelParserName = qvname "parseLabel"

regParserName, immParserName, src2ParserName, fregParserName, addressParserName :: Q Name
regParserName     = qvname "parseReg"
immParserName     = qvname "parseImm"
src2ParserName    = qvname "parseSrc2"
fregParserName    = qvname "parseFReg"
addressParserName = qvname "parseAddr"

labelCtorName, commentCtorName :: Q Name
labelCtorName   = qvname "MLabel"
commentCtorName = qvname "MComment"

makeMipsInstructionParser :: Q [Dec]
makeMipsInstructionParser = do
    let parserName = mkName "parseMipsInstruction"
    instParsers <- mkInstructionParsers
    return [ValD (VarP parserName) (NormalB $ combineParsers instParsers) []]
  where combineParsers :: [Exp] -> Exp
        combineParsers [final] = ParensE final
        combineParsers (p:ps) = UInfixE (ParensE p) (VarE '(<|>)) $ combineParsers ps

mkInstructionParsers :: Q [Exp]
mkInstructionParsers = do
    min <- mipsInstructionName
    TyConI (DataD _ _ _ _ ctors _) <- reify min
    let ctor_names = map extractName ctors
    mapM mkOneInstructionParser ctor_names
  where extractName (NormalC name _) = name

mkOneInstructionParser :: Name -> Q Exp
mkOneInstructionParser inst_name = do
    mipsInstTypeName <- mipsInstructionName
    DataConI _ type_info parentName <- reify inst_name
    when (mipsInstTypeName /= parentName) $
        fail $ "Data constructor " ++ show inst_name
               ++ " does not belong to type MipsInstruction"
    let arg_types = unpackFunType type_info
    argParsers <- mapM getParser arg_types
    let conNameStr = nameBase inst_name
        conMipsName = mipsNameOf conNameStr
    conMipsNameParser <- [| try (string $(litE (StringL conMipsName)) <* lookAhead space) |]
    buildInstructionParser
        (ConE $ mkName $ "P" ++ nameBase inst_name)
        conMipsNameParser
        argParsers

unpackFunType :: Type -> [Name]
unpackFunType (ConT mips_instruction) = []
unpackFunType (AppT (AppT ArrowT (ConT t)) rest) = t : unpackFunType rest

getParser :: Name -> Q Name
getParser arg = do
    rdest <- rdestName
    rsrc  <- rsrcName
    if arg == rdest || arg == rsrc then regParserName else do
    src2  <- src2Name
    if arg == src2 then src2ParserName else do
    frdest <- frdestName
    frsrc  <- frsrcName
    if arg == frdest || arg == frsrc then fregParserName else do
    imm <- immName
    if arg == imm then immParserName else do
    label <- labelName
    if arg == label then labelParserName else do
    address <- addressName
    if arg == address then addressParserName else
        fail $ "Can't construct parser for type " ++ show arg

mipsNameOf :: String -> String
mipsNameOf ('M':name) = start name
  where start (c : rest) = toLower c : walk rest
        walk [] = []
        walk (c : rest) | isUpper c = '.' : toLower c : walk rest
                        | otherwise = c : walk rest

buildInstructionParser :: Exp -> Exp -> [Name] -> Q Exp
buildInstructionParser ctor instParser [] =
    UInfixE instParser (VarE '(>>)) <$> [| return $(return ctor) |]
buildInstructionParser ctor instParser argParsers =
    let part1 = UInfixE instParser (VarE '(>>)) . UInfixE ctor (VarE '(<$>))
    in part1 <$> go (map VarE argParsers)
  where go :: [Exp] -> Q Exp
        go [p]    = skipSpace p
        go (p:ps) = (\a b -> UInfixE a (VarE '(<*>)) b) <$> skipSpace p <*> go1 ps

        go1 :: [Exp] -> Q Exp
        go1 [p] = commaSkipSpace p
        go1 (p:ps) = (\a b -> UInfixE a (VarE '(<*>)) b) <$> commaSkipSpace p <*> go1 ps

        skipSpace :: Exp -> Q Exp
        skipSpace e = ParensE <$> [| skipMany1 space *> $(return e) |]

        commaSkipSpace :: Exp -> Q Exp
        commaSkipSpace e =
            ParensE <$> [| char ',' *> skipMany1 space *> $(return e) |]
