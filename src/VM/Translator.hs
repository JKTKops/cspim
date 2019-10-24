{- MODULE VM.TRANSLATOR

This module contains a translator for cspim-vm code to MIPS assembly.
Semantics developed by Max Kopinsky in conjuction with Jing-Chen Peng and Ajay Tatachar.

See: VM.Language for abstract syntax of cspim-vm. MIPS.Language for abstract syntax of MIPS.

The important global note is how registers are allocated:
$t0 is the "argument pointer"
$t1 is the "local pointer"
$t2 is the "struct pointer"
$t3 is the "array pointer"
$t4 is the "accumulator".

The VM is a stack machine, so all the instructions manipulate the stack.
However, MIPS requires arithmetic to be done on registers. This means a huge portion
of instructions would be spent moving the top word of the stack to and from a register.
Instead, we maintain the top word of the stack in $t4. When double operations are performed,
the second word of the double comes from the actual top of the stack.

Doubles in MIPS are stored with the lower word in the lower address;
a double at addresses 0x00400000 and 0x00400001 has the lower word in 0x00400000
and the upper word in 0x00400001.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module VM.Translator where

import GHC.Exts (IsList(..))

import VM.Language
import MIPS.Language
import MIPS.Parser   (mips)

import Data.Array
import Data.Char
import Data.Word

import Control.Lens
import Control.Lens.TH

import Control.Monad.RWS
import Data.Functor (($>))

panic :: String -> a
panic msg = let formatted =
                    "PANIC: cspim has encountered an error and it doesn't know what to do.\n\
                    \cspim panicked during VM->MIPS translation, with message:\n" ++ msg
            in error formatted

-------------------------------------
-- Register pattern synonyms
--
-- Defined here because this is a translation detail that could change.
-------------------------------------

pattern ArgPtr    = RegS0
pattern LclPtr    = RegS1
pattern StructPtr = RegS2
pattern ArrayPtr  = RegS3
pattern Acc       = RegS4
pattern Tmp       = RegS5  -- a temporary register used for binary operations.
                           -- not saved, generally used like the assembler uses $at.

pattern StackPtr  = RegSP

-- | A non-total function that converts a VMS to Register.
--   ONLY call if you can prove the VMS is not Static or Pointer.
vms2Ptr :: VirtualMemorySegment -> Reg
vms2Ptr VmsArg    = ArgPtr
vms2Ptr VmsLocal  = LclPtr
vms2Ptr VmsStruct = StructPtr
vms2Ptr VmsArray  = ArrayPtr

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

data TranslationEnv = TE
     { _wholeProgram :: VmProgram
     , _ixBounds     :: (Word32, Word32)
     }

initTE :: VmProgram -> TranslationEnv
initTE prog = TE prog (bounds prog)

makeLenses ''TranslationEnv

data TranslationState = TS
     { _instIndex    :: Word32
     , _labelFresh   :: Int
     , _nArgs        :: Word32
     }

initTS :: TranslationState
initTS = TS 0 0 0

makeLenses ''TranslationState

newtype Translator a = Translator { unTranslator :: RWS
                                                      TranslationEnv
                                                      (DiffList MipsLine)
                                                      TranslationState
                                                      a }
  deriving ( Functor, Applicative, Monad, MonadState TranslationState
           , MonadReader TranslationEnv, MonadWriter (DiffList MipsLine))

translate :: VmProgram -> [MipsLine]
translate prog =
    let (_, _, dl) = runRWS (unTranslator translationDriver) (initTE prog) initTS
    in clean $ toList dl
  where clean :: [MipsLine] -> [MipsLine]
        clean = map cleanComment . filter (/= ML Nothing Nothing)

        cleanComment :: MipsLine -> MipsLine
        cleanComment (ML d (Just comment)) = ML d (Just (trim $ trim comment))
        cleanComment ml = ml

        trim :: String -> String
        trim = reverse . dropWhile isSpace

currentInst :: Translator VmInstruction
currentInst = do
    prog <- view wholeProgram
    ix <- use instIndex
    return $ prog Data.Array.! ix

freshLabel :: Translator String
freshLabel = do
    fr <- use labelFresh
    labelFresh += 1
    return $ "$L" ++ show fr

emitOne :: MipsLine -> Translator ()
emitOne mi = emit [mi]

emit :: [MipsLine] -> Translator ()
emit mls = tell $ fromList mls

translationDriver :: Translator ()
translationDriver = do
    ix <- use instIndex
    (lb, ub) <- views wholeProgram bounds
    unless (ix < lb || ix > ub) $ do
        inst <- currentInst
        case inst of
            VmPush _ _   -> push inst
            VmPop _ _    -> pop inst
            ControlFlow  -> controlFlow inst
            LogicOp      -> logicOp inst
            SignedComp   -> signedComp inst
            UnsignedComp -> unsignedComp inst
            FloatComp    -> floatComp inst
            DoubleComp   -> doubleComp inst
            SignedOp     -> signedArith inst
            UnsignedOp   -> unsignedArith inst
            FloatOp      -> floatArith inst
            DoubleOp     -> doubleArith inst
            VmTerminate  -> terminate
        instIndex += 1
        translationDriver

push :: VmInstruction -> Translator ()
-- Immediate pushes are always single words
push inst@(VmPush _ (Right imm)) = emit [mips|
sub $sp, $sp, 4
sw  ${Acc}, 0($sp)
li  ${Acc}, !{imm} # #{show inst}
|]

push (VmPush i_type (Left (VmAddress segment offset))) = case i_type of
    InstDouble -> pushDoubleFrom segment offset
    _          -> pushOtherFrom i_type segment offset

pushDoubleFrom :: VirtualMemorySegment -> Offset -> Translator ()
pushDoubleFrom vms offset = case vms of
    VmsStatic  -> panic "Push from static not implemented."
    VmsPointer -> panic "Push instruction attempts to push double from pointer segment"
    _          -> emit [mips|
sub $sp, $sp, 8                                 #N allocate 2 words
sw  ${Acc}, 4($sp)                              #N store accumulator in upper one
lw  ${Acc}, !{offset + 4}(${vms2Ptr vms})
sw  ${Acc}, 0($sp)                              #N Store upper word of double in top of stack
lw  ${Acc}, !{offset}(${vms2Ptr vms})           # push double #{show vms} #{show offset}
|]

pushOtherFrom :: InstType -> VirtualMemorySegment -> Offset -> Translator ()
pushOtherFrom i_type vms offset = case vms of
    VmsStatic  -> panic "Push from static not implemented."
    VmsPointer -> pushFromPointer offset
    _          -> do
        it <- currentInst
        emit [mips| sub $sp, $sp, 4    #N allocate 1 word
                    sw  ${Acc}, 0($sp) #N move acc to top of stack
                  |]
        case i_type of
            InstByte  -> emit [mips| lbu ${Acc}, !{offset}(${vms2Ptr vms}) # #{show it} |]
            InstShort -> emit [mips| lhu ${Acc}, !{offset}(${vms2Ptr vms}) # #{show it} |]
            _         -> emit [mips| lw  ${Acc}, !{offset}(${vms2Ptr vms}) # #{show it} |]

pushFromPointer :: Offset -> Translator ()
pushFromPointer offset =
    let reg = case offset of
            0 -> StructPtr
            1 -> ArrayPtr
            _ -> panic $
                     "Push instruction attempts to push from illegal Pointer segment offset "
                     ++ show offset
    in currentInst >>= \inst -> emit [mips|
sub  $sp,    $sp, 4     #N allocate 1 word
sw   ${Acc}, 0($sp)     #N move acc to top of stack
move ${Acc}, ${reg}     # #{show inst}
|]

pop :: VmInstruction -> Translator ()
pop (VmPop i_type (VmAddress segment offset)) = case i_type of
    InstDouble -> popDoubleTo segment offset
    _          -> popOtherTo i_type segment offset


popDoubleTo :: VirtualMemorySegment -> Offset -> Translator ()
popDoubleTo vms offset = case vms of
    VmsStatic  -> panic "Pop to static not implemented."
    VmsPointer -> panic "Pop instruction attempts to pop double to pointer segment"
    _          -> emit [mips|
sw   ${Acc}, !{offset}(${vms2Ptr vms})     #N pop lower word of double
lw   ${Acc}, 0($sp)                        #N pull upper word of double from stack
sw   ${Acc}, !{offset + 4}(${vms2Ptr vms}) #N pop upper word of double
lw   ${Acc}, 4($sp)                        #N pull top of stack into acc
add  $sp,    $sp, 8                        # pop double #{show vms} #{show offset}
|]

popOtherTo :: InstType -> VirtualMemorySegment -> Offset -> Translator ()
popOtherTo i_type vms offset = case vms of
    VmsStatic  -> panic "Pop to static not implemented."
    VmsPointer -> popToPointer offset
    _ -> do
        inst <- currentInst
        case i_type of
            InstByte  -> emit [mips| sb ${Acc}, !{offset}(${vms2Ptr vms}) |]
            InstShort -> emit [mips| sh ${Acc}, !{offset}(${vms2Ptr vms}) |]
            _         -> emit [mips| sw ${Acc}, !{offset}(${vms2Ptr vms}) |]
        emit [mips|
lw  ${Acc}, 0($sp)                        #N pull top of stack into acc
add $sp,    $sp, 4                        # #{show inst}
|]

popToPointer :: Offset -> Translator ()
popToPointer offset =
    let reg = case offset of
            0 -> StructPtr
            1 -> ArrayPtr
            _ -> panic $
                     "Pop instruction attempts to push from illegal pointer segment offset "
                     ++ show offset
    in currentInst >>= \inst -> emit [mips|
move ${reg}, ${Acc}     #N pop acc into ptr register
lw   ${Acc}, 0($sp)     #N pull top of stack into acc
add  $sp,    $sp, 4     # #{show inst}
|]

controlFlow :: VmInstruction -> Translator ()
controlFlow (VmLabel  label) = emit [mips| @{label}: |]
controlFlow (VmGoto   label) = emit [mips| j @{label} # goto #{label} |]
controlFlow (VmIfGoto label) =
    emit [mips|
move ${Tmp}, ${Acc}
lw   ${Acc}, 0($sp)
add  $sp, $sp, 4
bnez ${Tmp}, @{label} # ifgoto #{label}
|]
controlFlow (VmFunc label args lcls) = do
    emit [mips| @{label}: # start of function #{label} |]
    pushArgs args
    let plural = if lcls == 1 then "" else "s"
    savePtrs
    emit [mips| add  ${ArgPtr}, $sp, 20    # set arg ptr
                sub  $sp, $sp, !{4 * lcls} # make space for #{show lcls} local word#{plural}
                move ${LclPtr}, $sp        # set local ptr
              |]
    nArgs .= args
controlFlow (VmCall label args) = do
    if args == 0
      then emit [mips| sub $sp, $sp, 4 # 1 word for acc
                       sw  ${Acc}, 0($sp)
                     |]
      else popArgs args
    emit [mips| jal @{label}              # call #{label}
                move ${Acc},       $v0    # $acc = return value
              |]

controlFlow VmReturn = do
    args <- use nArgs
    emit [mips|
move $v0, ${Acc}
move ${Tmp}, ${ArgPtr}    # save arg ptr

sub  $sp, ${ArgPtr}, 20   # move $sp to saved values
lw   $ra, 0($sp)
lw   ${ArrayPtr}, 4($sp)
lw   ${StructPtr}, 8($sp)
lw   ${LclPtr}, 12($sp)
lw   ${ArgPtr}, 16($sp)   # restore saved values

add  $sp, ${Tmp}, !{4 * args} # use saved arg ptr to restore $sp
jr   $ra                          # return
|]

savePtrs :: Translator ()
savePtrs =
    emit [mips| sub $sp, $sp, 20          # 5 words for ptrs and $ra
                sw  ${ArgPtr},   16($sp)
                sw  ${LclPtr},   12($sp)
                sw  ${StructPtr}, 8($sp)
                sw  ${ArrayPtr},  4($sp)  # save ptrs
                sw  $ra,          0($sp)  # save $ra
              |]

pushArgs :: Word32 -> Translator ()
pushArgs n | n > 4 = pushArgs 4
           | n < 0 = pushArgs 0
pushArgs 0 = return ()
pushArgs 1 = emit [mips| sub $sp, $sp, 4
                         sw  $a0, 0($sp)
                       |]
pushArgs 2 = emit [mips| sub $sp, $sp, 8
                         sw  $a1, 4($sp)
                         sw  $a0, 0($sp)
                       |]
pushArgs 3 = emit [mips| sub $sp, $sp, 12
                         sw  $a2, 8($sp)
                         sw  $a1, 4($sp)
                         sw  $a0, 0($sp)
                       |]
pushArgs 4 = emit [mips| sub $sp, $sp, 16
                         sw  $a3, 12($sp)
                         sw  $a2, 8($sp)
                         sw  $a1, 4($sp)
                         sw  $a0, 0($sp)
                       |]

popArgs :: Word32 -> Translator ()
popArgs 0 = do
    when (n >= 1) $ emit [mips| move $a0, ${Acc}  |]
    when (n >= 2) $ emit [mips| lw   $a1, 0($sp)  |]
    when (n >= 3) $ emit [mips| lw   $a2, 4($sp)  |]
    when (n >= 4) $ emit [mips| lw   $a3, 8($sp) |]
    let moved = if n <= 1 then 0 else min (n - 1) 3
    when (moved > 0) $ emit [mips| add $sp, $sp, !{4 * moved} |]

shiftArgs :: Word32 -> Translator ()
shiftArgs 0 = return ()
shiftArgs nArgs = forM_ ([0 .. nArgs - 1] :: [Word32]) $ \n ->
    emit [mips| lw ${Tmp}, !{20 + 4 * n}($sp)
                sw ${Tmp}, !{4 * n}($sp)
              |]

{- [NOTE: Register Order in binops]:
the C code
x < y

translates into the VM (psuedo)code:
push x
push y
lt

translates into the mips code:
# push x to stack
# push y to accumulator
lw  $t5, 0($sp)    # pull x into temp register
add $sp, $sp, 4    # deallocate stack
slt $t4, $t5, $t4  # set acc = tmp < acc

Note the importance that tmp is the first argument, since it
was the argument which was pushed to the stack first.
-}

logicOp :: VmInstruction -> Translator ()
logicOp VmNot  = emit [mips| seq ${Acc}, ${Acc}, 0 # not |]
logicOp VmComp = emit [mips| not ${Acc}, ${Acc}    # comp |]
logicOp inst   = binOp >> case inst of
    VmAnd -> emit [mips| and ${Acc}, ${Tmp}, ${Acc} # and |]
    VmOr  -> emit [mips| or  ${Acc}, ${Tmp}, ${Acc} # or  |]
    VmNor -> emit [mips| nor ${Acc}, ${Tmp}, ${Acc} # nor |]
    VmXor -> emit [mips| xor ${Acc}, ${Tmp}, ${Acc} # xor |]

signedComp :: VmInstruction -> Translator ()
signedComp inst = binOp >> case inst of
    VmEq -> emit [mips| seq ${Acc}, ${Tmp}, ${Acc} # eq |]
    VmNe -> emit [mips| sne ${Acc}, ${Tmp}, ${Acc} # ne |]
    VmGe -> emit [mips| sge ${Acc}, ${Tmp}, ${Acc} # ge |]
    VmGt -> emit [mips| sgt ${Acc}, ${Tmp}, ${Acc} # gt |]
    VmLe -> emit [mips| sle ${Acc}, ${Tmp}, ${Acc} # le |]
    VmLt -> emit [mips| slt ${Acc}, ${Tmp}, ${Acc} # lt |]

unsignedComp :: VmInstruction -> Translator ()
unsignedComp inst = binOp >> case inst of
    VmLtu -> emit [mips| sltu ${Acc}, ${Tmp}, ${Acc} # ltu |]
    VmLeu -> emit [mips| sleu ${Acc}, ${Tmp}, ${Acc} # leu |]
    VmGtu -> emit [mips| sgtu ${Acc}, ${Tmp}, ${Acc} # gtu |]
    VmGeu -> emit [mips| sgeu ${Acc}, ${Tmp}, ${Acc} # geu |]

binOp :: Translator ()
binOp = emit [mips|
lw  ${Tmp}, 0($sp)
add $sp, $sp, 4
|]

floatComp :: VmInstruction -> Translator ()
floatComp inst = do
    floatBinOpSetup
    let com = show inst
    expected <- case inst of
        -- we can invert the registers rather than branching the other way
        VmEqS -> emit [mips| c.e.s  $f0, $f2 # #{com} |] $> True
        VmNeS -> emit [mips| c.e.s  $f0, $f2 # #{com} |] $> False
        VmGeS -> emit [mips| c.lt.s $f2, $f0 # #{com} |] $> True
        VmGtS -> emit [mips| c.le.s $f2, $f0 # #{com} |] $> True
        VmLeS -> emit [mips| c.le.s $f0, $f2 # #{com} |] $> True
        VmLtS -> emit [mips| c.lt.s $f0, $f2 # #{com} |] $> True
    conditionFlagJumpTable expected

floatBinOpSetup :: Translator ()
floatBinOpSetup = emit [mips|
mtc1 ${Acc}, $f2
l.s  $f0, 0($sp)
add  $sp, $sp, 4
|]

doubleComp :: VmInstruction -> Translator ()
doubleComp inst = do
    doubleBinOpSetup
    let com = show inst
    expected <- case inst of
        VmEqD -> emit [mips| c.e.d  $f0, $f2 # #{com} |] $> True
        VmNeD -> emit [mips| c.e.d  $f0, $f2 # #{com} |] $> False
        VmGeD -> emit [mips| c.lt.d $f2, $f0 # #{com} |] $> True
        VmGtD -> emit [mips| c.le.d $f2, $f0 # #{com} |] $> True
        VmLeD -> emit [mips| c.le.d $f0, $f2 # #{com} |] $> True
        VmLtD -> emit [mips| c.lt.d $f0, $f2 # #{com} |] $> True
    conditionFlagJumpTable expected

doubleBinOpSetup :: Translator ()
doubleBinOpSetup = emit [mips|
mtc1 ${Acc}, $f2
lwc1 $f3,    0($sp)
l.d  $f0,    4($sp)
add  $sp, $sp, 12
|]

conditionFlagJumpTable :: Bool -> Translator ()
conditionFlagJumpTable expected = do
    lblTr  <- freshLabel
    lblEnd <- freshLabel
    let com = "Jump table to set acc after fp comp"
    if expected
        then emit [mips| bc1t @{lblTr} # #{com} |]
        else emit [mips| bc1f @{lblTr} # #{com} |]
    emit [mips|
li  ${Acc}, 0
j   @{lblEnd}
@{lblTr}:
li  ${Acc}, 1
@{lblEnd}:   # End jump table.
|]

signedArith :: VmInstruction -> Translator ()
signedArith inst = do
    binOp
    let com = show inst
    case inst of
        VmAdd -> emit [mips| add  ${Acc}, ${Tmp}, ${Acc} # #{com} |]
        VmSub -> emit [mips| sub  ${Acc}, ${Tmp}, ${Acc} # #{com} |]
        VmMul -> emit [mips| mul  ${Acc}, ${Tmp}, ${Acc} # #{com} |]
        VmDiv -> emit [mips| div  ${Acc}, ${Tmp}, ${Acc} # #{com} |]
        VmSll -> emit [mips| sllv ${Acc}, ${Tmp}, ${Acc} # #{com} |]

unsignedArith :: VmInstruction -> Translator ()
unsignedArith inst = do
    binOp
    let com = show inst
    case inst of
        VmAddU -> emit [mips| addu  ${Acc}, ${Tmp}, ${Acc}, # #{com} |]
        VmSubU -> emit [mips| subu  ${Acc}, ${Tmp}, ${Acc}, # #{com} |]
        VmMulU -> emit [mips| mulou ${Acc}, ${Tmp}, ${Acc}, # #{com} |]
        VmDivU -> emit [mips| divu  ${Acc}, ${Tmp}, ${Acc}, # #{com} |]

floatArith :: VmInstruction -> Translator ()
floatArith inst = do
    floatBinOpSetup
    let com = show inst
    case inst of
        VmAddS -> emit [mips| add.s $f0, $f0, $f2 |]
        VmSubS -> emit [mips| sub.s $f0, $f0, $f2 |]
        VmMulS -> emit [mips| mul.s $f0, $f0, $f2 |]
        VmDivS -> emit [mips| div.s $f0, $f0, $f2 |]
    emit [mips| mfc1 ${Acc}, $f0 # #{com} |]

doubleArith :: VmInstruction -> Translator ()
doubleArith inst = do
    doubleBinOpSetup
    let com = show inst
    case inst of
        VmAddD -> emit [mips| add.d $f0, $f0, $f2 |]
        VmSubD -> emit [mips| sub.d $f0, $f0, $f2 |]
        VmMulD -> emit [mips| mul.d $f0, $f0, $f2 |]
        VmDivD -> emit [mips| div.d $f0, $f0, $f2 |]
    emit [mips| mfc1.d ${Acc}, $f0
                sub    $sp, $sp, 4
                sw     ${Tmp}, 0($sp) # #{com}
|]

terminate :: Translator ()
terminate = emit [mips|
li $v0, 10
syscall    # terminate
|]
