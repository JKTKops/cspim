{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module TAC.CodeGen where

import GHC.Exts (IsList (..))

import Pretty
import TAC.Program as Tac

import Compiler.Monad
import Compiler.SymbolTable

import MIPS.Language as Mips hiding (Label)
import MIPS.Parser (mips)

import Data.DList hiding (toList, fromList) -- imported from IsList already
import Data.Function ((&))
import Data.WordUtils
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Control.Monad.RWS.Strict
import Control.Lens

mipsCodeGenProc :: Program -> Compiler [MipsLine]
mipsCodeGenProc Prog{_functions = fns, _symbolTable = symtab} =
    let action = do -- written this way to make it easier to add constants and globalVars later
            mapM_ codeGenFunction fns
    in snd <$> unwrapCodeGen action symtab

data CGError = UniqueNotInMap String Unique deriving Show

instance Pretty CGError where
    pretty (UniqueNotInMap mapName u) =
        "Unique " ++ show u ++ " could not be found in symbol table: " ++ mapName ++ "."

newtype CodeGen a = CG { unCG :: (RWST SymbolTable (DList MipsLine) () Compiler) a }
  deriving ( Functor, Applicative, Monad, MonadCompiler
           , MonadReader SymbolTable, MonadWriter (DList MipsLine))

unwrapCodeGen :: CodeGen a -> SymbolTable -> Compiler (a, [MipsLine])
unwrapCodeGen (CG rwst) symtab =
    runRWST rwst symtab () >>= \(a, _, w) -> return (a, toList w)

emit :: DList MipsLine -> CodeGen ()
emit = tell

--------------------------------------------------------------------------------------
--
-- Compiler temporaries
--
--------------------------------------------------------------------------------------

-- NOTE [Compiler temporaries]: The compiler reserves a couple of temporaries for storing
-- the results of MIPS operations that need to be stored to the stack.

compilerAddrTemp :: Reg
compilerTemp1,  compilerTemp2  :: Reg
compilerFTemp1, compilerFTemp2 :: FReg -- need two registers to hold a double
compilerAddrTemp = RegAT
compilerTemp1  = RegT8
compilerTemp2  = RegT9
compilerFTemp1 = RegF0
compilerFTemp2 = RegF2

--------------------------------------------------------------------------------------
--
-- The major instruction dispatch - decide what case is being handled and invoke the
-- appropriate code generator.
--
--------------------------------------------------------------------------------------

-- NOTE: [Unoptimized Mips]
-- Hand optimizations are currently needed to eliminate redundant labels and the such.

insnCodeGen :: Insn e x -> CodeGen ()
insnCodeGen (Label l) = labelCodeGen l
insnCodeGen (Enter f) = enterCodeGen f
insnCodeGen (lvalue := rvalue) = assignCodeGen lvalue rvalue
insnCodeGen (Retrieve var) = retrieveCodeGen var
insnCodeGen (Goto l) = gotoCodeGen l
insnCodeGen (IfGoto rvalue t f) = ifgotoCodeGen rvalue t f
insnCodeGen (Call f args ret_label) = callCodeGen f args -- drop ret_label as only Hoopl cares
insnCodeGen (Return f mexp) = returnCodeGen f mexp

labelCodeGen :: Tac.Label -> CodeGen ()
labelCodeGen lbl = do
    lbl_name <- askLabelName lbl
    emit [mips| @{lbl_name}: |]

-- | Generates assembly for an 'enter f' instruction.
--   This assembly is called the "prologue" and builds the stack frame for the function.

-- TODO: Affected by leaf procedure flag
enterCodeGen :: Name -> CodeGen ()
enterCodeGen uniq = do
    func <- askFuncTable uniq
    let sf = func ^. stackFrame
        sf_size = sf ^. size
        sf_save_regs = sf ^. savedRegisters
        sf_pass_table = sf ^. passingTable
    emit [mips| addu $sp, $sp, !{ - sf_size } |]
    saveRegisters sf_save_regs
    emit [mips| move $fp, $sp |]
    storeArgs sf_pass_table

  where
    saveRegisters :: M.Map Reg MemLoc -> CodeGen ()
    saveRegisters map =
        let stackSaved = mapFilter isOffsetLoc map
        in sequence_ $ mapMapWithKey saveReg stackSaved
      where isOffsetLoc loc | OffsetLoc _ <- loc = True
                            | otherwise          = False

    saveReg :: Reg -> MemLoc -> CodeGen ()
    saveReg reg (OffsetLoc off) =
        emit [mips| sw ${reg}, !{off}($sp) |]

    storeArgs :: UniqueMap MemLoc -> CodeGen ()
    storeArgs = sequence_ . mapMapWithKey storeArg

    storeArg :: Name -> MemLoc -> CodeGen ()
    storeArg var desired_loc = do
        (loc, ty) <- askMemLocType var
        assignVarToVar loc ty desired_loc ty

{- NOTE: [Prologue/return sensitivity]

Since the prologue and return are repsonsible for moving the frame and stack pointers around,
we have to be careful with how the frame pointer and return address are saved (they are in the
saved registers map of every stack frame). In particular, They must be saved before $fp is
moved, and restored after, while other variables must be saved after $fp is moved and
restored before.
-}

--------------------------------------------------------------------------------------
--
-- Code generation for Assign (:=) instructions
--
--------------------------------------------------------------------------------------

-- | Generates assembly for an assignment (:=) instruction.
--   There are many cases to consider. RValues need to be computed, which is affected
--   by their types. LValues and RValues can both be arrays, which requires loading the offset
--   and adjusting it for alignment.
assignCodeGen :: LValue -> RValue -> CodeGen ()
assignCodeGen lvalue rvalue =
    case rvalue of
        RVar (Left runiq) -> do
            (rMemLoc, rType) <- askMemLocType runiq
            case lvalue of
                LVar luniq -> do
                    (lMemLoc, lType) <- askMemLocType luniq
                    assignVarToVar lMemLoc lType rMemLoc rType
        RVar (Right const) ->
            case lvalue of
                LVar luniq -> do
                    (lMemLoc, lType) <- askMemLocType luniq
                    assignConstToVar lMemLoc lType const

assignVarToVar :: MemLoc -> Type -> MemLoc -> Type -> CodeGen ()
assignVarToVar lloc ltype rloc rtype
    | isIntegralTy ltype && isIntegralTy rtype = case (lloc, rloc) of
          (OffsetLoc loff, OffsetLoc roff) -> do
              -- l{w,h,b} $compilerTemp1, roff($fp)
              -- s{w,h,b} $compilerTemp1, loff($fp)
              retrieveInst <- integralLoadInst rtype <@> compilerTemp1 <@> Right (roff, RegFP)
              storeInst   <- integralStoreInst ltype <@> compilerTemp1 <@> Right (loff, RegFP)
              emit $ fromList [instToLine retrieveInst, instToLine storeInst]
          (RegLoc reg, OffsetLoc off) -> do
              -- l{w,h,b} $reg, off($fp)
              inst <- integralLoadInst rtype <@> reg <@> Right (off, RegFP)
              emit $ singleton $ instToLine inst
          (OffsetLoc off, RegLoc reg) -> do
              -- s{w,h,b} $reg, off($fp)
              inst <- integralStoreInst ltype <@> reg <@> Right (off, RegFP)
              emit $ singleton $ instToLine inst
          (RegLoc lr, RegLoc rr) -> emit [mips| move ${lr}, ${rr} |]
          _ -> panic "Int types being assigned to/from F registers!"

assignConstToVar :: MemLoc -> Type -> Constant -> CodeGen ()
assignConstToVar vloc vtype const
    | isIntTy vtype = case vloc of
          (OffsetLoc off) -> emit
              (if intValueOfConst const == 0
              then [mips| sw $zero, !{off}($fp) |]
              else [mips| add ${compilerTemp1}, $0, !{intValueOfConst const}
                          sw ${compilerTemp1}, !{off}($fp)
                        |])
          (RegLoc reg) -> emit [mips| add ${reg}, $0, !{intValueOfConst const} |]

-- | Generates assembly for the (LIxArr n ix := RValue) case.
assignToArr :: Name -> Var -> RValue -> CodeGen ()
assignToArr uniq var rv = panic "assignment to arrays not implemented"

integralLoadInst :: Type -> CodeGen (RDest -> Address -> MipsInstruction)
integralLoadInst (IntTy _) = pure MLw
integralLoadInst (ShortTy _) = pure MLh
integralLoadInst (CharTy _) = pure MLb
integralLoadInst _ = panic "TAC.CodeGen.integralLoadInst: Non-integral type!"

integralStoreInst :: Type -> CodeGen (RSrc -> Address -> MipsInstruction)
integralStoreInst (IntTy _) = pure MSw
integralStoreInst (ShortTy _) = pure MSh
integralStoreInst (CharTy _) = pure MSb
integralStoreInst _ = panic "TAC.CodeGen.integralStoreInst: Non-integral type!"

-- | Generates code for a 'retrieve v' instruction.
retrieveCodeGen :: Name -> CodeGen ()
retrieveCodeGen uniq = panic "retreive not implemented"

gotoCodeGen :: Label -> CodeGen ()
gotoCodeGen lbl = do
    lbl_name <- askLabelName lbl
    tell [mips| j @{lbl_name} |]

ifgotoCodeGen :: RValue -> Label -> Label -> CodeGen ()
ifgotoCodeGen rvalue lbl_t lbl_f = panic "ifgoto not implemented"

{- case rvalue of
    RVar (Left uniq)   -> branchVar uniq lbl_t lbl_f
    RVar (Right const) -> branchConst const lbl_t lbl_f
    RIxArr uniq ix     -> branchArrIx uniq ix lbl_t lbl_f
    Binop v1 op v2     -> branchBinop op v1 v2 lbl_t lbl_f
    Monop op v         -> branchMonop op v lbl_t lbl_f
-}

callCodeGen f_uniq args = panic "call not implemented"

-- | Generate code for return statements. This includes setting $v0 and the jr $ra instruction.
--   Destroys the function's frame restoring the frame pointer,
--   but does /not/ clean the stack arguments. This is the responsibility of the caller.
returnCodeGen :: Name -> Maybe RValue -> CodeGen ()
returnCodeGen uniq mrval = do
    case mrval of
        Nothing -> pure ()
        Just _ -> panic "returning expressions not implemented yet"
    func <- askFuncTable uniq
    let sf_size = func ^. stackFrame.size
        saved_regs = func ^. stackFrame.savedRegisters
    emit [mips| move $sp, $fp |]
    loadRegs saved_regs
    emit [mips| addu $sp, $sp, !{sf_size}
                jr $ra
              |]

  where loadRegs :: M.Map Reg MemLoc -> CodeGen ()
        loadRegs map =
            let stackSaved = mapFilter isOffsetLoc map
            in sequence_ $ mapMapWithKey loadReg stackSaved
          where isOffsetLoc loc | OffsetLoc _ <- loc = True
                                | otherwise          = False

        loadReg :: Reg -> MemLoc -> CodeGen ()
        loadReg reg (OffsetLoc off) =
            emit [mips| lw ${reg}, !{off}($sp) |]


--------------------------------------------------------------------------------------
--
-- Entrypoints to the major instruction dispatcher
--
--------------------------------------------------------------------------------------

-- TODO: emit a .globl directive!
codeGenFunction :: Function -> CodeGen ()
codeGenFunction f =
    let blockInsns = insnsInFunction f
    in mapM_ codeGenBlockInsns blockInsns

codeGenBlockInsns :: BlockInsns -> CodeGen ()
codeGenBlockInsns (e, ms, x) = insnCodeGen e >> mapM_ insnCodeGen ms >> insnCodeGen x

--------------------------------------------------------------------------------------
--
-- Utilities for working with raw instructions
--
--------------------------------------------------------------------------------------

instToLine :: MipsInstruction -> MipsLine
instToLine mi = ML (Just (MInst mi)) Nothing

--------------------------------------------------------------------------------------
--
-- Utilities for the CodeGen Monad (should be defined in separate file?)
--
--------------------------------------------------------------------------------------

class SymTabKey k where
    notFound :: String -> k -> CGError

instance SymTabKey Int where -- Unique
    notFound = UniqueNotInMap

askCodeGen :: SymTabKey k => String -> AskSymTabM k v -> k -> CodeGen v
askCodeGen n req key = do
    mv <- req key
    case mv of
        Nothing -> panic $ pretty $ notFound n key
        Just v  -> return v

{-
askLabelName lbl = askLabelNameM lbl >>= \case
    Just name -> pure name
    Nothing   -> pure $ show lbl

fmap combined with 'maybe' is more efficient
-}
askLabelName :: Tac.Label -> CodeGen String
askLabelName lbl = fromMaybe ('$' : show lbl) <$> askLabelNameM lbl

askVarName :: Unique -> CodeGen String
askVarName = askCodeGen "var names" askVarNameM

askVarType :: Unique -> CodeGen Type
askVarType = askCodeGen "var types" askVarTypeM

askFuncTable :: Unique -> CodeGen Function
askFuncTable = askCodeGen "function table" askFuncTableM

askMemLoc :: Unique -> CodeGen MemLoc
askMemLoc = askCodeGen "allocation table" askMemLocM

askMemLocType :: Unique -> CodeGen (MemLoc, Type)
askMemLocType uniq = (,) <$> askMemLoc uniq Prelude.<*> askVarType uniq
