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

data CGError = UniqueNotInMap Text Unique deriving Show

instance Pretty CGError where
    pretty (UniqueNotInMap mapName u) =
        "Unique " <> pretty (show u) <> " could not be found in symbol table: "
        <> mapName <> "."

newtype CodeGen a = CG { unCG :: (RWST SymbolTable (DList MipsLine) () Compiler) a }
  deriving ( Functor, Applicative, Monad, MonadCompiler
           , MonadReader SymbolTable, MonadWriter (DList MipsLine))

unwrapCodeGen :: CodeGen a -> SymbolTable -> Compiler (a, [MipsLine])
unwrapCodeGen (CG rwst) symtab =
    runRWST rwst symtab () <&> \(a, _, w) -> (a, toList w)

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
insnCodeGen (SetRV rvalue) = setRVCodeGen rvalue
insnCodeGen (Goto l) = gotoCodeGen l
insnCodeGen (IfGoto rvalue t f) = ifgotoCodeGen rvalue t f
insnCodeGen (Call f args ret_label) = callCodeGen f args -- drop ret_label as only Hoopl cares
insnCodeGen (Return f) = returnCodeGen f

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
assignCodeGen :: LValue -> TacExp -> CodeGen ()
assignCodeGen lvalue exp =
    case exp of
        (ValExp (RVar (Left runiq))) -> do
            (rMemLoc, rType) <- askMemLocType runiq
            case lvalue of
                LVar luniq -> do
                    (lMemLoc, lType) <- askMemLocType luniq
                    assignVarToVar lMemLoc lType rMemLoc rType
        (ValExp (RVar (Right const))) ->
            case lvalue of
                LVar luniq -> do
                    (lMemLoc, lType) <- askMemLocType luniq
                    assignConstToVar lMemLoc lType const

        b@(Binop _ _ _) -> binopCodeGen lvalue b

assignVarToVar :: MemLoc -> Type -> MemLoc -> Type -> CodeGen ()
assignVarToVar lloc ltype rloc rtype
    | isIntegralTy ltype && isIntegralTy rtype = case (lloc, rloc) of
          (OffsetLoc loff, OffsetLoc roff) -> do
              -- l{w,h,hu,b,bu} $compilerTemp1, roff($fp)
              -- s{w,h,b} $compilerTemp1, loff($fp)
              retrieveInst <- integralLoadInst rtype <@> compilerTemp1 <@> Right (roff, RegFP)
              storeInst   <- integralStoreInst ltype <@> compilerTemp1 <@> Right (loff, RegFP)
              emit $ fromList [instToLine retrieveInst, instToLine storeInst]
          (RegLoc reg, OffsetLoc off) -> do
              -- l{w,h,hu,b,bu} $reg, off($fp)
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
    | isIntegralTy vtype = case vloc of
          (OffsetLoc off) ->
              (if intValueOfConst const == 0
                  -- s{w,h,b} $0, off($fp)
              then integralStoreInst vtype <@> Reg0 <@> Right (off, RegFP) >>=
                       emit . singleton . instToLine
              else do
                      -- add $compilerTemp1, $0, !{intValueOfConst const}
                      -- s{w,h,b} $compilerTemp1, off($fp)
                      emit [mips| add ${compilerTemp1}, $0, !{intValueOfConst const} |]
                      store <- integralStoreInst vtype <@> compilerTemp1 <@> Right (off, RegFP)
                      emit $ singleton $ instToLine store)
          (RegLoc reg) -> emit [mips| add ${reg}, $0, !{intValueOfConst const} |]

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
    comm <- prettyInsn $ lvalue := b
    (destReg, storeCode) <- chooseDestReg lvalue
    emit $ instruction destReg leftRsrc rightSrc2
    emit   storeCode


  where getLeft (RVar (Left lu)) = loadNameWithDefault compilerTemp1 lu
        getLeft (RVar (Right (IntConst c))) = do
            emit [mips| li ${compilerTemp1}, !{fromIntegral c} |]
            return compilerTemp1
        getLeft (RVar (Right _)) = panic "binopCodeGen: left operand is non-integral constant"

        getRight (RVar (Left ru)) = Left <$> loadNameWithDefault compilerTemp2 ru
        getRight (RVar (Right (IntConst c))) = return $ Right $ fromIntegral c
        getRight (RVar (Right _)) = panic "binopCodeGen: right operand is non-integral constant"

        chooseDestReg (LVar u) = ask >>= \r ->
            CG $ lift $ do
                (reg, _, code) <- runRWST (unCG $ storeNameWithDefault compilerTemp1 u) r ()
                return (reg, code)

        loadNameWithDefault :: Reg -> Name -> CodeGen Reg
        loadNameWithDefault = handleNameWithDefaultReg integralLoadInst

        storeNameWithDefault :: Reg -> Name -> CodeGen Reg
        storeNameWithDefault = handleNameWithDefaultReg integralStoreInst

        handleNameWithDefaultReg :: (Type -> CodeGen (Reg -> Address -> MipsInstruction))
                                 -> Reg -> Name -> CodeGen Reg
        handleNameWithDefaultReg instFor def u = do
            (loc, ty) <- askMemLocType u
            case loc of
                OffsetLoc off -> do
                    handler <- instFor ty
                    emit [instToLine $ handler def $ Right (off, RegFP)]
                    return def
                RegLoc reg -> return reg
                _ -> panic "binopCodeGen: floating point/global variables not implemented"

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


binopCodeGen _ _ = panic $ "binopCodeGen: RValue isn't a binop!"

-- | Generates assembly for the (LIxArr n ix := RValue) case.
assignToArr :: Name -> Var -> RValue -> CodeGen ()
assignToArr uniq var rv = panic "assignment to arrays not implemented"

integralLoadInst :: Type -> CodeGen (RDest -> Address -> MipsInstruction)
integralLoadInst (IntTy _) = pure MLw
integralLoadInst (ShortTy Signed)   = pure MLh
integralLoadInst (ShortTy Unsigned) = pure MLhu
integralLoadInst (CharTy Signed)    = pure MLb
integralLoadInst (CharTy Unsigned)  = pure MLbu
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
setRVCodeGen :: RValue -> CodeGen ()
setRVCodeGen (RVar (Left uniq)) = do
    (memloc, ty) <- askMemLocType uniq
    assignVarToVar (RegLoc RegV0) ty memloc ty
setRVCodeGen (RVar (Right const))
  | IntConst i <- const = assignConstToVar (RegLoc RegV0) (IntTy signage) const
  | otherwise = panic "returning non-integral constants not implemented"
  where signage = if intValueOfConst const < 0 then Signed else Unsigned

-- | Generate code for returning from a function. Reloads saved registers,
--   and cleans up the stack frame. Does /not/ set $v0; SetRV instructions set the return value.
returnCodeGen :: Name -> CodeGen ()
returnCodeGen uniq = do
    func <- askFuncTable uniq
    let sf_size = func ^. stackFrame.size
        saved_regs = func ^. stackFrame.savedRegisters
    emit [mips| move $sp, $fp # Start return sequence |]
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

-- TODO: Don't emit a .globl directive for static functions
codeGenFunction :: Function -> CodeGen ()
codeGenFunction f = do
    let blockInsns = insnsInFunction f
    funName <- askVarName $ f ^. name
    emit [ ML (Just (MDirective $ DotText))          Nothing
         , ML (Just (MDirective $ DotGlobl funName)) Nothing
         ]
    mapM_ codeGenBlockInsns blockInsns

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
