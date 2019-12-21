{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
module TAC.CodeGen where

import Pretty
import TAC.Pretty (prettyInsn)
import TAC.Program as Tac

import Compiler.Monad
import Compiler.SymbolTable

import MIPS.Language as Mips hiding (Program, Label)
import MIPS.Parser (mips)

import Data.DList
import Data.Function ((&))
import Data.Functor (($>))
import Data.WordUtils
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Map as M

import Control.Monad.RWS.Strict
import Control.Monad.Fail
import Control.Lens

mipsCodeGenProc :: Program -> Compiler [MipsLine]
mipsCodeGenProc Prog{_functions = fns, _symbolTable = symtab} =
    let action = do -- written this way to make it easier to add constants and globalVars later
            mapM_ codeGenFunction fns
    in snd <$> unwrapCodeGen action symtab

data CGError = UniqueNotInMap Text Unique deriving Show

instance Pretty CGError where
    pretty (UniqueNotInMap mapName u) =
        "Unique " <> pretty (show u) <> " could not be found in symbol table: "
        <> mapName <> "."

newtype CodeGen a = CG { unCG :: (RWST SymbolTable (DList MipsLine) CodeGenState Compiler) a }
  deriving ( Functor, Applicative, Monad, MonadCompiler
           , MonadReader SymbolTable, MonadWriter (DList MipsLine), MonadState CodeGenState)

newtype CodeGenState = CGS { returnLabel :: Maybe Tac.Label }
initCodeGenState = CGS Nothing

instance MonadFail CodeGen where
    fail = panic . pack

unwrapCodeGen :: CodeGen a -> SymbolTable -> Compiler (a, [MipsLine])
unwrapCodeGen (CG rwst) symtab =
    runRWST rwst symtab initCodeGenState <&> \(a, _, w) -> (a, toList w)

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
compilerTemp1  = RegV0
compilerTemp2  = RegV1
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
-- insnDispatch :: Insn e x -> CodeGen ()
-- insnDispatch insn = do
--     comm <- prettyInsn insn
--     emit [mips| ##{comm} |]
--     insnCodeGen insn

insnCodeGen :: Insn e x -> CodeGen ()
insnCodeGen (Label l) = labelCodeGen l
insnCodeGen (Enter f) = enterCodeGen f
insnCodeGen (lvalue := rvalue) = assignCodeGen lvalue rvalue
insnCodeGen (Retrieve var) = retrieveCodeGen var
insnCodeGen (Goto l) = gotoCodeGen l
insnCodeGen (IfGoto rvalue t f) = ifgotoCodeGen rvalue t f
insnCodeGen (Call f args ret_label) = callCodeGen f args -- drop ret_label as only Hoopl cares
insnCodeGen (Return mrv) = returnCodeGen mrv

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
        when (desired_loc /= loc) $ do
            reg <- integralLoadMemLocTypeWithDefault desired_loc ty compilerTemp1
            integralStoreMemLocType reg ty loc

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
assignCodeGen :: LValue -> TacExp -> CodeGen ()
assignCodeGen lvalue exp =
    case exp of
        (ValExp rvalue) -> do
            dest <- integralLoadRValueWithDefault rvalue compilerTemp1
            integralStoreLValue dest lvalue
        b@(Binop _ _ _) -> binopCodeGen lvalue b

{- NOTE: [Mul instructions]

Spim does overflow checking for mulo and mulou instructions, which we don't want to perform.
Mostly because those checks take ~5 instructions and cause an exception which takes more
instructions to ignore. Instead, we need to explicitly generate the code

mult $v0, $v1
mflo $v0

-}
-- | Generate assembly for a binop. Panics if the given 'RValue' is not 'Binop'.
--   Will cause a /cspim/ crash if either rvalue is of floating point type!
binopCodeGen :: LValue -> TacExp -> CodeGen ()
binopCodeGen lvalue b@(Binop lvar op rvar) = do
    leftRsrc <- getLeft lvar
    leftSignage <- getSignage lvar
    rightSignage <- getSignage rvar
    rightSrc2 <- getRight rvar
    instruction <- selectBinopInst op leftSignage rightSignage
    destReg <- chooseDestReg lvalue
    emit $ instruction destReg leftRsrc rightSrc2
    integralStoreLValue destReg lvalue
binopCodeGen _ _ = panic $ "binopCodeGen: RValue isn't a binop!"

getLeft :: RValue -> CodeGen Reg
getLeft (RVar (Left lu)) = integralLoadNameWithDefault lu compilerTemp1
getLeft (RVar (Right (IntConst c))) = do
    emit [mips| li ${compilerTemp1}, !{fromIntegral c} |]
    return compilerTemp1
getLeft (RVar (Right _)) = panic "getLeft: operand is non-integral constant"

getRight :: RValue -> CodeGen (Either Reg Imm)
getRight (RVar (Left ru)) = Left <$> integralLoadNameWithDefault ru compilerTemp2
getRight (RVar (Right (IntConst c))) = return $ Right $ fromIntegral c
getRight (RVar (Right _)) = panic "getRight: right operand is non-integral constant"

-- | Given an LValue, choose the best register to send a result to this lvalue
--   and also return the code for storing from that register.
chooseDestReg :: LValue -> CodeGen Reg
chooseDestReg (LVar u) = do
    askMemLoc u <&> \case
        RegLoc r -> r
        _        -> compilerTemp1

selectBinopInst :: Binop -> Signage -> Signage -- signages of the operands
                -> CodeGen (RDest -> RSrc -> Src2 -> DList MipsLine)
selectBinopInst op lsign rsign = return $ case op of
    Add -> instToLines -< MAddu
    Sub -> instToLines -< MSubu
    Mul -> mkMulCode
    Div -> mkDivCode
    Rem -> mkRemCode
    ShiftR | lsign == Signed -> instToLines -< MSra
           | otherwise       -> instToLines -< MSrl
    ShiftL              -> instToLines -< MSll
    Le | eitherUnsigned -> instToLines -< MSleu
       | otherwise      -> instToLines -< MSle
    Lt | eitherUnsigned -> instToLines -< MSltu
       | otherwise      -> instToLines -< MSlt
    Ge | eitherUnsigned -> instToLines -< MSgeu
       | otherwise      -> instToLines -< MSge
    Gt | eitherUnsigned -> instToLines -< MSgtu
       | otherwise      -> instToLines -< MSgt
    Eq                  -> instToLines -< MSeq
    Ne                  -> instToLines -< MSne

  where eitherUnsigned = case (lsign, rsign) of
            (Signed, Signed) -> False
            _                -> True

        (-<) :: (a -> b) -> (c -> d -> e -> a) -> c -> d -> e -> b
        (-<) f g c d e = f (g c d e)
        infixr 9 -<

        mkMulCode :: RDest -> RSrc -> Src2 -> DList MipsLine
        mkMulCode dest src1 src2 = fmap instToLine insts
          where insts = src2Insts <> multInsts
                (src2Insts, src2Reg) = case src2 of
                    Left reg -> ([], reg)
                    Right imm -> ([MLi compilerTemp2 imm], compilerTemp2)
                multInsts = [MMult src1 src2Reg, MMflo dest]

        mkDivCode dest src1 src2 =
            [instToLine $ (if eitherUnsigned then MDivu else MDiv) dest src1 src2]

        mkRemCode dest src1 src2 =
            [instToLine $ (if eitherUnsigned then MRemu else MRem) dest src1 src2]


getSignage :: RValue -> CodeGen Signage
getSignage (RVar (Left u)) = signageOf <$> askVarType u
getSignage (RVar (Right _)) = return Signed


-- | Generates code for a 'retrieve v' instruction.
retrieveCodeGen :: Name -> CodeGen ()
retrieveCodeGen uniq = integralStoreName RegV0 uniq

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
returnCodeGen :: Maybe RValue -> CodeGen ()
returnCodeGen (Just rv) = do
    integralLoadRValue rv RegV0
    jumpToReturnLabel
returnCodeGen Nothing = jumpToReturnLabel

-- | Generate code for returning from a function. Reloads saved registers,
--   and cleans up the stack frame. Does /not/ set $v0; Return instructions set $v0 and
--   then jump here.
leaveFunCodeGen :: Name -> CodeGen ()
leaveFunCodeGen uniq = do
    func <- askFuncTable uniq
    let sf_size = func ^. stackFrame.size
        saved_regs = func ^. stackFrame.savedRegisters
    returnLabel <- useReturnLabel
    emit [mips| @{returnLabel}: # Start return sequence |]
    emit [mips| move $sp, $fp |]
    loadRegs saved_regs
    emit [mips| addu $sp, $sp, !{sf_size}
                jr $ra |]

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

-- TODO: Don't emit a .globl directive for static functions
codeGenFunction :: Function -> CodeGen ()
codeGenFunction f = do
    let blockInsns = insnsInFunction f
    funName <- askVarName $ f ^. name
    emit [ ML (Just (MDirective $ DotText))          Nothing
         , ML (Just (MDirective $ DotGlobl funName)) Nothing
         ]
    freshReturnLabel
    mapM_ codeGenBlockInsns blockInsns
    leaveFunCodeGen $ f ^. name
    clearReturnLabel

codeGenBlockInsns :: BlockInsns -> CodeGen ()
codeGenBlockInsns (e, ms, x) = insnCodeGen e >> mapM_ insnCodeGen ms >> insnCodeGen x

--------------------------------------------------------------------------------------
--
-- Utilities for working with raw instructions
--
--------------------------------------------------------------------------------------

instToLine :: MipsInstruction -> MipsLine
instToLine mi = ML (Just (MInst mi)) Nothing

instToLines :: MipsInstruction -> DList MipsLine
instToLines mi = [ML (Just (MInst mi)) Nothing]

--------------------------------------------------------------------------------------
--
-- Utilities for the CodeGen Monad (should be defined in separate file?)
--
--------------------------------------------------------------------------------------

class SymTabKey k where
    notFound :: Text -> k -> CGError

instance SymTabKey Int where -- Unique
    notFound = UniqueNotInMap

askCodeGen :: SymTabKey k => Text -> AskSymTabM k v -> k -> CodeGen v
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

labelForVar :: Unique -> CodeGen String
labelForVar uniq = do
    name <- askVarName uniq
    return $ name ++ "." ++ show uniq

jumpToReturnLabel :: CodeGen ()
jumpToReturnLabel = do
    lbl <- useReturnLabel
    emit [mips| j @{lbl} |]

useReturnLabel :: CodeGen String
useReturnLabel = do
    Just lbl <- gets returnLabel
    askLabelName lbl

freshReturnLabel :: CodeGen ()
freshReturnLabel = do
    lbl <- freshLabel
    modify $ \s -> s { returnLabel = Just lbl }

clearReturnLabel :: CodeGen ()
clearReturnLabel = modify $ \s -> s { returnLabel = Nothing }

--------------------------------------------------------------------------------------
--
-- Data movement instructions
--
--------------------------------------------------------------------------------------

integralLoadInst :: Type -> CodeGen (RDest -> Address -> MipsInstruction)
integralLoadInst (IntTy _) = pure MLw
integralLoadInst (ShortTy Signed)   = pure MLh
integralLoadInst (ShortTy Unsigned) = pure MLhu
integralLoadInst (CharTy Signed)    = pure MLb
integralLoadInst (CharTy Unsigned)  = pure MLbu
integralLoadInst ty = panic $ "TAC.CodeGen.integralLoadInst: Non-integral type! "
                           <> "(" <> pretty ty <> ")"

integralStoreInst :: Type -> CodeGen (RSrc -> Address -> MipsInstruction)
integralStoreInst (IntTy _) = pure MSw
integralStoreInst (ShortTy _) = pure MSh
integralStoreInst (CharTy _) = pure MSb
integralStoreInst ty = panic $ "TAC.CodeGen.integralStoreInst: Non-integral type! "
                            <> "(" <> pretty ty <> ")"

-- | Load the value in a particular memory location and with a given type. Default to using
--   the given register as the destination. Returns the register where the variable was
--   actually stored.
integralLoadMemLocTypeWithDefault :: MemLoc -> Type -> Reg -> CodeGen Reg
integralLoadMemLocTypeWithDefault (OffsetLoc off) ty defReg = do
    loadInst <- integralLoadInst ty
    emit [instToLine $ loadInst defReg $ Right (off, RegFP)]
    return defReg
integralLoadMemLocTypeWithDefault (GPLoc _ uniq) ty defReg = do
    loadInst <- integralLoadInst ty
    lbl      <- labelForVar uniq
    emit [instToLine $ loadInst defReg $ Left (lbl, Nothing)]
    return defReg
integralLoadMemLocTypeWithDefault (DataLoc uniq) ty defReg = do
    loadInst <- integralLoadInst ty
    lbl      <- labelForVar uniq
    emit [instToLine $ loadInst defReg $ Left (lbl, Nothing)]
    return defReg
integralLoadMemLocTypeWithDefault (RegLoc reg) _ _ = return reg
integralLoadMemLocTypeWithDefault (FRegLoc _) _ _ =
    panic "TAC.CodeGen.integralLoadMemLocTypeWithDefault: FRegLoc!"

-- | Load the value from a given memory location, with the given type, into the given register.
integralLoadMemLocType :: MemLoc -> Type -> Reg -> CodeGen ()
integralLoadMemLocType memloc ty dest = do
    r <- integralLoadMemLocTypeWithDefault memloc ty dest
    when (r /= dest) $ emit [mips| move ${dest}, ${r} |]

-- | Load a variable into a register, using the given register as a default destination.
--   Returns the register where the variable can be found.
--   The result is only ever not the default if the variable's MemLoc is a register.
integralLoadNameWithDefault :: Unique -> Reg -> CodeGen Reg
integralLoadNameWithDefault u reg = do
    (memloc, ty) <- askMemLocType u
    integralLoadMemLocTypeWithDefault memloc ty reg

-- | Load a variable into the given register. If the result just needs to be in /some/
--   register, 'integralLoadNameWithDefault' can often omit moves here.
integralLoadName :: Unique -> Reg -> CodeGen ()
integralLoadName uniq reg = do
    (memloc, ty) <- askMemLocType uniq
    integralLoadMemLocType memloc ty reg

-- | Load a pointer to a given memory location into a given register.
--   Panics if the memory location is a register location.
loadMemLocPtr :: MemLoc -> Reg -> CodeGen ()
loadMemLocPtr (RegLoc _) _ =
    panic "TAC.CodeGen.loadMemLocPtr: Can't load pointer to register"
loadMemLocPtr (FRegLoc _) _ =
    panic "TAC.CodeGen.loadMemLocPtr: Can't load pointer to f-register"
loadMemLocPtr (OffsetLoc off) reg = emit [mips| addu ${reg}, $fp, !{off} |]
loadMemLocPtr (GPLoc _ uniq)  reg = do
    lbl <- labelForVar uniq
    emit [mips| la ${reg}, @{lbl} |]
loadMemLocPtr (DataLoc uniq) reg = do
    lbl <- labelForVar uniq
    emit [mips| la ${reg}, @{lbl} |]

loadNamePtr :: Unique -> Reg -> CodeGen ()
loadNamePtr u reg = do
    memloc <- askMemLoc u
    loadMemLocPtr memloc reg

integralLoadFromArray :: MemLoc -> Type -> Offset -> Reg -> CodeGen Reg
integralLoadFromArray (OffsetLoc base_off) ty arr_off dest =
    integralLoadMemLocTypeWithDefault (OffsetLoc $ base_off + arr_off) ty dest
integralLoadFromArray (GPLoc _ u) ty arr_off dest = do
    lbl <- labelForVar u
    inst <- integralLoadInst ty
    emit $ instToLines $ inst dest $ Left (lbl, Just arr_off)
    return dest
integralLoadFromArray (DataLoc u) ty arr_off dest = do
    lbl <- labelForVar u
    inst <- integralLoadInst ty
    emit $ instToLines $ inst dest $ Left (lbl, Just arr_off)
    return dest
integralLoadFromArray _ _ _ _ = panic "TAC.CodeGen.integralLoadFromArray: RegLoc"


-- | Load an RValue into a register. If the RValue is itself stored in a register, then
--   no code is emitted and that register is returned. Otherwise, the RValue is loaded into
--   the given register, and the given register is returned.
integralLoadRValueWithDefault :: RValue -> Reg -> CodeGen Reg
integralLoadRValueWithDefault (RVar (Right (IntConst i))) def =
    emit [mips| li ${def}, !{fromIntegral i} |] $> def
integralLoadRValueWithDefault (RVar (Left u)) def = do
    (memloc, ty) <- askMemLocType u
    case ty of
        ArrTy _ _ -> loadMemLocPtr memloc def $> def
        _         -> integralLoadMemLocTypeWithDefault memloc ty def
integralLoadRValueWithDefault (RDeref uniq off) def = do
    (memloc, ty) <- askMemLocType uniq
    case ty of
        ArrTy _ elemTy -> integralLoadFromArray memloc elemTy off def
        _         -> do
            dest <- integralLoadMemLocTypeWithDefault memloc ty def
            emit [mips| lw ${dest}, !{off}(${dest}) |]
            return dest

-- | Load an RValue into the given register. If you just need the value to be in /some/
--   register, then 'integralLoadRValueWithDefault' can ocassionally omit move instructions.
integralLoadRValue :: RValue -> Reg -> CodeGen ()
integralLoadRValue rv dest = do
    reg <- integralLoadRValueWithDefault rv dest
    when (dest /= reg) $ emit [mips| move ${dest}, ${reg} |]

-- | Store the value in the given reg, which has the given type, to the given memloc.
integralStoreMemLocType :: Reg -> Type -> MemLoc -> CodeGen ()
integralStoreMemLocType reg ty (OffsetLoc off) = do
    storeInst <- integralStoreInst ty
    emit [instToLine $ storeInst reg $ Right (off, RegFP)]
integralStoreMemLocType reg ty (GPLoc _ uniq) = do
    storeInst <- integralStoreInst ty
    lbl       <- labelForVar uniq
    emit [instToLine $ storeInst reg $ Left (lbl, Nothing)]
integralStoreMemLocType reg ty (DataLoc uniq) = do
    storeInst <- integralStoreInst ty
    lbl       <- labelForVar uniq
    emit [instToLine $ storeInst reg $ Left (lbl, Nothing)]
integralStoreMemLocType reg _ (RegLoc destreg) =
    when (reg /= destreg) $ emit [mips| move ${destreg}, ${reg} |]
integralStoreMemLocType _ _ (FRegLoc _) =
    panic "TAC.CodeGen.integralStoreMemLocTypeWithDefault: FRegLoc!"

-- | Store the value in the given register to the given variable.
integralStoreName :: Reg -> Unique -> CodeGen ()
integralStoreName r u = do
    (memloc, ty) <- askMemLocType u
    integralStoreMemLocType r ty memloc

integralStoreToArray :: Type -> Reg -> Unique -> Offset -> CodeGen ()
integralStoreToArray elemTy reg uniq off = do
    mkAddr <- getArrayAddr uniq
    inst <- integralStoreInst elemTy
    emit $ instToLines $ inst reg $ mkAddr $ Just off

getArrayAddr :: Unique -> CodeGen (Maybe Offset -> Address)
getArrayAddr u = do
    memloc <- askMemLoc u
    lbl <- labelForVar u
    case memloc of
        OffsetLoc off -> return $ \arr_off -> Right (off + fromMaybe 0 arr_off, RegFP)
        GPLoc _ _     -> return $ \arr_off -> Left (lbl, arr_off)
        DataLoc _     -> return $ \arr_off -> Left (lbl, arr_off)
        _ -> panic "TAC.CodeGen.getArrayAddr: (F)RegLoc"

-- TODO: censor the output of the codegen monad to add .set at and .set noat
-- | Store the value in a given register to the given LValue.
--   May make use of $at if the LValue is an LDeref.
integralStoreLValue :: Reg -> LValue -> CodeGen ()
integralStoreLValue r (LVar u) = integralStoreName r u
integralStoreLValue r (LDeref u off) = do
    ty <- askVarType u
    case ty of
        ArrTy _ elemTy -> integralStoreToArray elemTy r u off
        -- TODO: Add PtrTy

