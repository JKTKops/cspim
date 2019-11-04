{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module TAC.CodeGen where

import GHC.Exts (IsList (..))

import TAC.Program as Tac
import Compiler.SymbolTable
import MIPS.Language as Mips hiding (Label)
import MIPS.Parser (mips)

import Data.Function ((&))
import Data.WordUtils

import Control.Monad.Except
import Control.Monad.RWS.Strict

mipsCodeGenProc :: Program -> Either CGError [MipsLine]
mipsCodeGenProc Prog{_functions = fns, _symbolTable = symtab} =
    let action = do -- written this way to make it easier to add constants and globalVars later
            mapM_ codeGenFunction fns
    in case unwrapCodeGen action symtab of
        Left err      -> Left err
        Right (_, dl) -> Right $ toList dl

--------------------------------------------------------------------------------------
--
-- Difference Lists for efficient appending during emission
--
--------------------------------------------------------------------------------------

newtype DiffList a = DL { unDL :: [a] -> [a] }

instance IsList (DiffList a) where
    type Item (DiffList a) = a
    fromList l = DL $ \tail -> l ++ tail
    toList (DL f) = f []

instance Semigroup (DiffList a) where
    DL f <> DL g = DL (f . g)

instance Monoid (DiffList a) where
    mappend = (<>)
    mempty  = DL id

instance Show a => Show (DiffList a) where
    show (DL xs ) = "DL " ++ show (xs [])

data CGError = UniqueNotInMap String Unique
             | Panic String

instance Show CGError where
    show (UniqueNotInMap mapName u) =
        "Unique " ++ show u ++ " could not be found in symbol table: " ++ mapName ++ "."
    show (Panic s) = s

newtype CodeGen a = CG { unCG :: ExceptT CGError (RWS SymbolTable (DiffList MipsLine) ()) a }
  deriving ( Functor, Applicative, Monad, MonadError CGError
           , MonadReader SymbolTable, MonadWriter (DiffList MipsLine))

unwrapCodeGen :: CodeGen a -> SymbolTable -> Either CGError (a, [MipsLine])
unwrapCodeGen (CG erws) symtab =
    let f = runExceptT erws & runRWS
        (a, _, w) = f symtab ()
    in case a of
        Left err  -> Left err
        Right val -> Right (val, toList w)

emit :: DiffList MipsLine -> CodeGen ()
emit = tell

panic :: String -> CodeGen a
panic = throwError . Panic

--------------------------------------------------------------------------------------
--
-- Compiler temporaries
--
--------------------------------------------------------------------------------------

-- NOTE [Compiler temporaries]: The compiler reserves a couple of temporaries for storing
-- the results of MIPS operations that need to be stored to the stack.

compilerTemp1,  compilerTemp2  :: Reg
compilerFTemp1, compilerFTemp2 :: FReg -- need two registers to hold a double
compilerTemp1  = RegT5
compilerTemp2  = RegT6
compilerFTemp1 = RegF12
compilerFTemp2 = RegF13

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
enterCodeGen :: Name -> CodeGen ()
enterCodeGen uniq = panic "enter instruction not implemented"

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
    | isIntTy ltype && isIntTy rtype = case (lloc, rloc) of
          (OffsetLoc loff, OffsetLoc roff) -> emit
            [mips| lw ${compilerTemp1}, !{roff}($fp)
                   sw ${compilerTemp1}, !{loff}($fp)
                 |]
          (RegLoc reg, OffsetLoc off) -> emit [mips| lw ${reg}, !{off}($sp) |]
          (OffsetLoc off, RegLoc reg) -> emit [mips| sw ${reg}, !{off}($sp) |]
          (RegLoc lr, RegLoc rr) ->      emit [mips| move ${lr}, ${rr} |]
          _ -> panic "Int types being assigned to/from F registers!"

assignConstToVar :: MemLoc -> Type -> Constant -> CodeGen ()
assignConstToVar vloc vtype const
    | isIntTy vtype = case vloc of
          (OffsetLoc off) -> emit
                      [mips| li ${compilerTemp1}, !{intValueOfConst const}
                             sw ${compilerTemp1}, !{off}($fp)
                           |]
          (RegLoc reg) -> emit [mips| li ${reg}, !{intValueOfConst const} |]

-- | Generates assembly for the (LIxArr n ix := RValue) case.
assignToArr :: Name -> Var -> RValue -> CodeGen ()
assignToArr uniq var rv = panic "assignment to arrays not implemented"

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
returnCodeGen uniq mrval = emit [mips| jr $ra |]--panic "return not implemented"

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
-- Utilities for the CodeGen Monad (should be defined in separate file?)
--
--------------------------------------------------------------------------------------

invertOffset :: MemLoc -> MemLoc
invertOffset (OffsetLoc o) = OffsetLoc (-o)
invertOffset ml = ml

class SymTabKey k where
    notFound :: String -> k -> CGError

instance SymTabKey Int where -- Unique
    notFound = UniqueNotInMap

askCodeGen :: SymTabKey k => String -> AskSymTabM k v -> k -> CodeGen v
askCodeGen n req key = do
    mv <- req key
    case mv of
        Nothing -> throwError $ notFound n key
        Just v  -> return v

{-
askLabelName lbl = askLabelNameM lbl >>= \case
    Just name -> pure name
    Nothing   -> pure $ show lbl

fmap combined with 'maybe' is more efficient
-}
askLabelName :: Tac.Label -> CodeGen String
askLabelName lbl = maybe (show lbl) id <$> askLabelNameM lbl

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

--------------------------------------------------------------------------------------
--
-- Breaking the program down to initiate code generation

-- Much of this is copied or inspired by ghc/compiler/cmmUtils which provides the
-- toBlockListEntryFirstFalseFallthrough function used by the LLVM code generator.
--
--------------------------------------------------------------------------------------

-- | Get the body out of a C/C graph.
toBlockMap :: TacGraph -> Body Insn -- Body Insn ~ LabelMap (TacBlock)
toBlockMap TacGraph{_graph = GMany NothingO bodyMap NothingO} = bodyMap
-- There are no other cases because we know the graph is C/C.

-- | Gets a list of all the blocks in a TacGraph. No promises are made about order.
toBlockList :: TacGraph -> [TacBlock]
toBlockList g = mapElems $ toBlockMap g

-- | Like 'toBlockList', but the entry block always comes first.
toBlockListEntryFirst :: TacGraph -> [TacBlock]
toBlockListEntryFirst g
  | mapNull m = []
  | otherwise = entry_block : others
  where
    m = toBlockMap g
    entry_id = _entry g
    Just entry_block = mapLookup entry_id m
    others = filter ((/= entry_id) . entryLabel) (mapElems m)

-- | Like 'toBlockListEntryFirst', but we order the blocks so that the true case
-- of a conditional branches to the next block in the output list of blocks.
-- This lets us avoid emitting jump instructions for the true case!
-- Note that we are relying on the order of successors returned for IfGoto by the
-- NonLocal instance of Insn (defined in TAC/Language.hs) which is [f, t].
toBlockListEntryFirstTrueFallthrough :: TacGraph -> [TacBlock]
toBlockListEntryFirstTrueFallthrough g@TacGraph{_entry = entLbl} =
    postorder_dfs_from (toBlockMap g) entLbl

type BlockInsns = (Insn C O, [Insn O O], Insn O C)

insnsInBlock :: TacBlock -> BlockInsns
insnsInBlock blk =
    let (hd, tl) = blockSplitHead blk
        (middle, end) = blockSplitTail tl
    in (hd, blockToList middle, end)

insnsInFunction :: Function -> [BlockInsns]
insnsInFunction Fn{_body = g} = map insnsInBlock $ toBlockListEntryFirstTrueFallthrough g
