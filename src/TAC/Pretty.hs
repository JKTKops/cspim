{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module TAC.Pretty where

import GHC.Exts (Constraint)

import Pretty
import MIPS.Pretty () -- import instances
import Text.PrettyPrint hiding ((<>))

import TAC.Program
import Compiler.SymbolTable

import Data.Maybe
import Data.Functor
import Control.Arrow (first)
import Control.Monad.Reader
import Control.Lens

panic :: String -> String -> a
panic tag msg = error $ tag ++ ": Program invariant violated! " ++ msg

type SymTabReader m = MonadReader SymbolTable m

class Ppr a where
    ppr :: SymTabReader m => a -> m Doc

runPpr :: Ppr a => SymbolTable -> a -> Doc
runPpr tab a = runReader (ppr a) tab

prettyProgram :: Program -> Doc
prettyProgram Prog{_functions = fns, _globalVars = glbls, _symbolTable = symTab} =
    flip runReader symTab $ do
    fnDocs <- mapM ppr fns
    glbls  <- mapM (pprAssocAlloc Allocate <=< uniq2Assoc) glbls
    return $
         text "# ============ Globals ============="
      $$ vcat (map (char '#' <+>) glbls)
      $+$ text ""
      $+$ foldr ($+$) mempty (punctuate (text "") fnDocs)


pprFunction :: SymTabReader m => Function -> m Doc
pprFunction fn@Fn{_name = n, _args = as, _locals = lcls, _stackFrame = sf, _body = b} = do
    name <- ppr n
    sfComment <- map (char '#' <+>) <$> pprStackFramePieces sf as lcls
    b <- ppr b
    return $ header name $$ vcat sfComment $$ b
  where header name = text "# ============ Function ============"
                   $$ text "# Name:" <+> name

        pprStackFramePieces sf args lcls =
            let sregs = mapToList $ sf ^. savedRegisters
                sfHeader = text "Stack frame size:" <+> int (fromIntegral $ sf ^. size)
                allocations = liftM2 (++) (pprSRegs sregs) $ mapM pprAllocUniq (args ++ lcls)
            in (sfHeader :) <$> allocations

        pprSRegs = mapM (pprAssocAlloc Save . first pretty)
        pprAllocUniq = pprAssocAlloc Allocate <=< uniq2Assoc

uniq2Assoc :: SymTabReader m => Unique -> m (String, MemLoc)
uniq2Assoc u = do
    n <- askVarName u
    memloc <- fromMaybe (panic "uniq2Assoc" $ n ++ " was not allocated!")
                <$> askMemLocM u
    return (n, memloc)

data StorageReason = Save | Allocate
pprAssocAlloc :: SymTabReader m => StorageReason -> (String, MemLoc) -> m Doc
pprAssocAlloc b (name, memloc) = do
    let descriptor = case b of
            Save     -> " is saved at    "
            Allocate -> " is allocated to"
    prettyLoc <- ppr memloc
    return $ text name $$ nest 19 (text descriptor <+> prettyLoc)

pprMemloc :: SymTabReader m => MemLoc -> m Doc
pprMemloc = \case
    OffsetLoc i  -> pure $ int (fromIntegral i) <> text "($fp)"
    RegLoc reg   -> pure $ text $ pretty reg
    FRegLoc freg -> pure $ text $ pretty freg
    GPLoc b n -> do
        let directive = if b then ".extern" else ".lcomm"
        name <- askVarName n
        ty   <- fromMaybe (panic "pprMemloc" $ "type of " ++ name ++ " is unknown")
                  <$> askVarTypeM n
        return $ text (directive ++ " " ++ name ++ "_" ++ show n) <+> int (sizeof ty)

pprTacGraph :: SymTabReader m => TacGraph -> m Doc
pprTacGraph t_graph =
    let blocks = toBlockListEntryFirstTrueFallthrough t_graph
        blockInsnss = map insnsInBlock blocks
    in vcat <$> mapM pprBlockInsns blockInsnss

pprBlockInsns :: SymTabReader m => BlockInsns -> m Doc
pprBlockInsns (label, middles, end) = do
    label <- ppr label
    middles <- sequence $ ppr <$> middles
    end <- ppr end
    return $ label $+$ nest 8 (vcat $ middles ++ [end])

pprInsn :: SymTabReader m => Insn e x -> m Doc
pprInsn (Label lbl) = askLabelName lbl <&> \name -> text (name ++ ":")
pprInsn (Enter f)   = askVarName f <&> \name -> text ("enter " ++ name)
pprInsn (lv := rv)  = (\lft rght -> lft <+> text ":=" <+> rght) <$> ppr lv <*> ppr rv
pprInsn (Retrieve n) = askVarName n <&> \name -> text ("retrieve " ++ name)
pprInsn (SetRV rv) = do
    rv <- ppr rv
    return $ text "set return value to" <+> rv
pprInsn (Goto lbl)  = askLabelName lbl <&> \name -> text ("goto " ++ name)
pprInsn (IfGoto rv t f) = do
    test <- ppr rv
    true <- ppr t
    false <- ppr f
    return $ text "if" <+> test <+> text "goto" <+> true <+> text "else" <+> false
pprInsn (Call f args ret_label) = do
    f <- ppr f
    args <- sequence $ ppr <$> args
    rl <- ppr ret_label
    return $ text "call" <+> (f <> parens (hsep $ punctuate comma args))
         <+> text "and return to" <+> rl
pprInsn (Return f) = do
    f <- ppr f
    return $ text "return from" <+> f


pprLValue :: SymTabReader m => LValue -> m Doc
pprLValue (LVar name) = ppr name
pprLValue (LIxArr name ix) = do
    name <- ppr name
    ix <- ppr ix
    return $ name <> brackets ix

pprRValue :: SymTabReader m => RValue -> m Doc
pprRValue (RVar var) = ppr var
pprRValue (RIxArr name ix) = (\name ix -> name <> brackets ix) <$> ppr name <*> ppr ix
pprRValue (Binop l op r) = liftM2 (<+>) (liftM2 (<+>) (ppr l) (ppr op)) (ppr r)
pprRValue (Monop op var) = liftM2 (<>) (ppr op) (ppr var)

pprSignage :: Applicative f => Signage -> f Doc
pprSignage Signed   = pure $ text "signed"
pprSignage Unsigned = pure $ text "unsigned"

instance Pretty Program where pretty = render . prettyProgram
instance Ppr Function   where ppr = pprFunction
instance Ppr MemLoc     where ppr = pprMemloc
instance Ppr TacGraph   where ppr = pprTacGraph
instance Ppr (Insn e x) where ppr = pprInsn
instance Ppr LValue     where ppr = pprLValue
instance Ppr RValue     where ppr = pprRValue
instance Ppr Binop      where ppr = pure . text . show
instance Ppr Monop      where ppr = pure . text . show
instance Ppr Signage    where ppr = pprSignage

instance Ppr (Either Name Constant) where
    ppr (Left name) = ppr name
    ppr (Right const) = case const of
        IntConst i -> pure $ integer i
        FloatConst d -> pure $ double d
        StringConst s -> pure $ text $ show s

instance Ppr Int where
    ppr i = text <$> askVarName i

instance Ppr Label where
    ppr lbl = text <$> askLabelName lbl

askVarName :: SymTabReader m => Unique -> m String
askVarName n = askVarNameM n >>= \case
    Nothing -> return $ "$genName_" ++ show n
    Just name -> return name

askLabelName :: SymTabReader m => Label -> m String
askLabelName lbl = fromMaybe (show lbl) <$> askLabelNameM lbl
