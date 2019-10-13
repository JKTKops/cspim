{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
module VM.Emulator (emulate) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Array hiding ((!))
import qualified Data.Array as Array
import Data.Array.ST
import Data.Array.Unsafe
import GHC.ST

import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as Except

import Control.Lens

import VM.Language

-- Perform binary conversions between word and FP types.


castST :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
castST x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

wordToFloat :: Word32 -> Float
wordToFloat x = runST $ castST x

floatToWord :: Float -> Word32
floatToWord x = runST $ castST x

wordToDouble :: Word64 -> Double
wordToDouble x = runST $ castST x

doubleToWord :: Double -> Word64
doubleToWord x = runST $ castST x

wordToInt :: Word32 -> Int32
wordToInt = fromIntegral

intToWord :: Int32 -> Word32
intToWord = fromIntegral

-- | Combine two Word32s into one Word64.
-- Big Endian - the first word is the upper 32 bits.
combineWord32s :: (Word32, Word32) -> Word64
combineWord32s (w1, w2) =
    let pad :: Word32 -> Word64
        pad = fromIntegral
     in (pad w1 `shiftL` 32) .|. pad w2

-- | Split a Word64 into two Word32s.
-- Big Endian - the first word of the result is the upper 32 bits.
splitWord64 :: Word64 -> (Word32, Word32)
splitWord64 w =
    let upperMask :: Word64
        upperMask = 0xFFFFFFFF00000000

        lowerMask :: Word64
        lowerMask = 0x00000000FFFFFFFF

     in (fromIntegral $ (w .&. upperMask) `shiftR` 32, fromIntegral $ w .&. lowerMask)

-- Actual emulation stuff

type Memory = Map Pointer Word32
type LabelTable = Map String Word32

data EmuState = EmuState
    { _memory         :: Memory
    , _labelTable     :: LabelTable
    , _insts          :: VmProgram
    , _programCounter :: Word32
    , _stackPtr       :: Word32
    , _staticPtr      :: Word32
    , _localPtr       :: Word32
    , _argPtr         :: Word32
    , _structPtr      :: Word32
    , _arrayPtr       :: Word32
    }

makeLenses ''EmuState

initStackPtr, initStaticPtr :: Word32
initStackPtr  = 0x80000000
initStaticPtr = 0x10000000

initState :: VmProgram -> LabelTable -> EmuState
initState prog table =
    EmuState
      { _memory         = Map.empty
      , _labelTable     = table
      , _insts          = prog
      , _programCounter = 0
      , _stackPtr       = initStackPtr
      , _staticPtr      = initStaticPtr
      , _localPtr       = 0
      , _argPtr         = 0
      , _structPtr      = 0
      , _arrayPtr       = 0
      }

data EmuError = PopEmptyStack
              | DereferenceNull
              | ReadUninitializedMemory
              | InvalidPointerOffset Word32
              | PushDoubleFromPointer
              | PopDoubleToPointer
              | UnalignedAddress Word32
              | NoSuchLabel String
              | BehaviorNotImplemented String
              deriving (Eq, Show)

newtype Emulator a = Emulator { unEmulator :: ExceptT
                                                (EmuError, VmInstruction, Word32)
                                                (State EmuState)
                                                a }
  deriving ( Functor, Applicative, Monad
           , MonadError (EmuError, VmInstruction, Word32), MonadState EmuState)

throwError :: EmuError -> Emulator a
throwError e = do
    pc <- use programCounter
    inst <- uses insts (Array.! pc)
    Except.throwError (e, inst, pc)

runEmulator :: Emulator a -> VmProgram -> LabelTable
            -> Either (EmuError, VmInstruction, Word32) a
runEmulator emu prog table = flip evalState (initState prog table) . runExceptT $ unEmulator emu

emulate :: VmProgram -> IO ()
emulate prog =
    let labels    = filter (\(_, inst) -> isLabel inst) $ assocs prog
        assocList = map (\(offset, VmLabel label) -> (label, offset)) labels
        table     = Map.fromList assocList
    in case runEmulator execEmulator prog table of
           Left err    -> putStrLn "STATE VIOLATION:" >> putStrLn (formatErr err)
           Right stack -> do
               putStrLn "Program terminated successfully"
               putStrLn "Final stack (top of the stack is at the bottom):"
               mapM_ print $ reverse stack

execEmulator :: Emulator [Word32]
execEmulator = do
    while ((&&) <$> stepEmulator <*> validPC)
    sp <- use stackPtr
    uses memory $ \m -> map (m Map.!) [sp, sp + 4 .. initStackPtr - 4]
  where validPC = inBounds <$> uses insts bounds <*> use programCounter
        inBounds (a, b) c = a <= c && c <= b

while :: Monad m => m Bool -> m ()
while body = do
    b <- body
    when b $ while body

pushStack :: Word32 -> Emulator ()
pushStack word = do
    stackPtr -= 4
    stackPtr ~> writeMemory word

popStack :: Emulator Word32
popStack = do
    s <- use stackPtr
    if s == initStackPtr
      then throwError PopEmptyStack
      else do
        word <- readMemory s
        stackPtr += 4
        return word

peekStack :: Emulator (Maybe Word32)
peekStack = do
    s <- use stackPtr
    if s == initStackPtr
      then return Nothing
      else Just <$> readMemory s

readMemory :: Word32 -> Emulator Word32
readMemory addr = do
    when (addr == 0) $ throwError DereferenceNull
    when (addr `mod` 4 /= 0) $ throwError $ UnalignedAddress addr
    h <- use memory
    case h Map.!? addr of
        Nothing   -> throwError ReadUninitializedMemory
        Just word -> return word

writeMemory :: Word32 -> Word32 -> Emulator ()
writeMemory dat addr = do
    when (addr == 0) $ throwError DereferenceNull
    when (addr `mod` 4 /= 0) $ throwError $ UnalignedAddress addr
    memory %= Map.insert addr dat

stepEmulator :: Emulator Bool
stepEmulator = do
    pc <- use programCounter
    instruction <- uses insts (Array.! pc)

    let exec = case instruction of
            VmTerminate -> return False
            _ -> do case instruction of
                        VmPush i_type src  -> push i_type src
                        VmPop  i_type dest -> pop  i_type dest

                        ControlFlow  -> stepControl    instruction
                        LogicOp      -> stepLogicOp    instruction
                        SignedComp   -> stepIntComp    instruction
                        UnsignedComp -> stepIntComp    instruction
                        FloatComp    -> stepFloatComp  instruction
                        DoubleComp   -> stepDoubleComp instruction
                        SignedOp     -> stepSignedOp   instruction
                        UnsignedOp   -> stepUnsignedOp instruction
                        FloatOp      -> stepFloatOp    instruction
                        DoubleOp     -> stepDoubleOp   instruction
                    return True

    exec <* (programCounter += 1)

push :: InstType -> Either VmAddress Word32 -> Emulator ()
push InstDouble (Right imm) = throwError $ BehaviorNotImplemented "Pushing double immediates"
push _          (Right imm) = pushImmediate imm
push InstDouble (Left addr) = pushAddressDouble addr
push _          (Left addr) = pushAddress addr

pushImmediate :: Word32 -> Emulator ()
pushImmediate word = pushStack word

pushAddress :: VmAddress -> Emulator ()
pushAddress (VmAddress segment offset) = case segment of
    VmsStatic  -> pushFrom staticPtr offset
    VmsArg     -> pushFrom argPtr offset
    VmsLocal   -> pushFrom localPtr offset
    VmsPointer -> case offset of
        0 -> structPtr ~> pushStack
        1 -> arrayPtr  ~> pushStack
        n -> throwError $ InvalidPointerOffset n
    VmsStruct  -> pushFrom structPtr offset
    VmsArray   -> pushFrom arrayPtr offset

  where pushFrom :: Getting Pointer EmuState Pointer -> Offset -> Emulator ()
        pushFrom ptr_getter offset = do
            basePtr <- use ptr_getter
            memVal  <- readMemory $ basePtr + 4 * offset
            pushStack memVal

pushAddressDouble :: VmAddress -> Emulator ()
pushAddressDouble (VmAddress segment offset) = case segment of
    VmsStatic  -> pushFrom staticPtr offset
    VmsArg     -> pushFrom argPtr offset
    VmsLocal   -> pushFrom localPtr offset
    VmsPointer -> throwError PushDoubleFromPointer
    VmsStruct  -> pushFrom structPtr offset
    VmsArray   -> pushFrom arrayPtr offset

  where pushFrom :: Getting Pointer EmuState Pointer -> Offset -> Emulator ()
        pushFrom ptr_getter offset = do
            basePtr <- use ptr_getter
            upper   <- readMemory $ basePtr + 4 * (offset + 1)
            lower   <- readMemory $ basePtr + 4 * offset
            pushStack upper
            pushStack lower

pop :: InstType -> VmAddress -> Emulator ()
pop i_type (VmAddress segment offset) = case segment of
    VmsStatic  -> popTo staticPtr offset
    VmsArg     -> popTo argPtr offset
    VmsLocal   -> popTo localPtr offset
    VmsPointer -> do
        when (i_type == InstDouble) $ throwError PopDoubleToPointer
        case offset of
            0 -> structPtr <~ popStack
            1 -> arrayPtr  <~ popStack
            n -> throwError $ InvalidPointerOffset n
    VmsStruct  -> popTo structPtr offset
    VmsArray   -> popTo arrayPtr offset

  where popTo :: Getting Pointer EmuState Pointer -> Offset -> Emulator ()
        popTo ptr_getter offset = do
            basePtr <- use ptr_getter
            when (i_type == InstDouble) $ do
                lower <- popStack
                writeMemory lower $ basePtr + 4 * offset
            word <- popStack
            let offset' = if i_type == InstDouble then offset + 1 else offset
            writeMemory word $ basePtr + 4 * offset'

type Step = VmInstruction -> Emulator ()
stepControl, stepLogicOp                                :: Step
stepIntComp, stepFloatComp, stepDoubleComp              :: Step
stepSignedOp, stepUnsignedOp, stepFloatOp, stepDoubleOp :: Step

stepControl = \case
    VmLabel  label -> return ()
    VmGoto   label -> jump label
    VmIfGoto label -> do
        x <- popStack
        when (x /= 0) $ jump label
  where jump label = do
            targetPC <- uses labelTable $ \lt -> lt Map.!? label
            case targetPC of
                Nothing  -> throwError $ NoSuchLabel label
                Just pc  -> programCounter .= pc

logicNot :: Word32 -> Word32
logicNot w | w == 0    = 1
           | otherwise = 0

stepLogicOp VmNot = popStack >>= pushStack . logicNot
stepLogicOp inst =
    let op = case inst of
            VmAnd  -> (.&.)
            VmOr   -> (.|.)
            VmNor  -> \x y -> logicNot $ x .|. y
            VmXor  -> xor
    in do x <- popStack
          y <- popStack
          pushStack $ x `op` y

stepIntComp inst =
    let op = case inst of
            VmEq -> (==)
            VmNe -> (==)
            VmLt -> intComp (<)
            VmLe -> intComp (<=)
            VmGt -> intComp (>)
            VmGe -> intComp (>=)

            VmLtu -> (<)
            VmLeu -> (<=)
            VmGtu -> (>)
            VmGeu -> (>=)
    in do x <- popStack
          y <- popStack
          pushStack .logic $ y `op` x

stepFloatComp inst =
    let op = case inst of
            VmEqS -> (==)
            VmNeS -> (/=)
            VmGtS -> (>)
            VmGeS -> (>=)
            VmLtS -> (<)
            VmLeS -> (<=)
    in do x <- popStack
          y <- popStack
          pushStack . logic $ wordToFloat y `op` wordToFloat x

stepDoubleComp inst =
    let op = case inst of
            VmEqD -> (==)
            VmNeD -> (/=)
            VmGtD -> (>)
            VmGeD -> (>=)
            VmLtD -> (<)
            VmLeD -> (<=)
    in do x_lower <- popStack
          x_upper <- popStack
          y_lower <- popStack
          y_upper <- popStack
          let y = wordToDouble $ combineWord32s (y_upper, y_lower)
              x = wordToDouble $ combineWord32s (x_upper, x_lower)
          pushStack . logic $ y `op` x

stepSignedOp inst =
    let op = case inst of
            VmAdd -> (+)
            VmSub -> (-)
            VmMul -> (*)
            VmDiv -> div
    in do x <- popStack
          y <- popStack
          let x_signed = fromIntegral x :: Int32
              y_signed = fromIntegral y :: Int32
          pushStack . fromIntegral $ y_signed `op` x_signed

stepUnsignedOp inst =
    let op = case inst of
            VmAddU -> (+)
            VmSubU -> (-)
            VmMulU -> (*)
            VmDivU -> div
     in do x <- popStack
           y <- popStack
           pushStack $ y `op` x

stepFloatOp inst =
    let op = case inst of
            VmAddS -> (+)
            VmSubS -> (-)
            VmMulS -> (*)
            VmDivS -> (/)
     in do x <- popStack
           y <- popStack
           let x_float = wordToFloat x
               y_float = wordToFloat y
           pushStack . floatToWord $ y_float `op` x_float

stepDoubleOp inst =
    let op = case inst of
            VmAddD -> (+)
            VmSubD -> (-)
            VmMulD -> (*)
            VmDivD -> (/)
     in do x_lower <- popStack
           x_upper <- popStack
           y_lower <- popStack
           y_upper <- popStack
           let x_double = wordToDouble $ combineWord32s (x_upper, x_lower)
               y_double = wordToDouble $ combineWord32s (y_upper, y_lower)
               r_double = y_double `op` x_double
               (r_upper, r_lower) = splitWord64 $ doubleToWord r_double
           pushStack r_upper
           pushStack r_lower

-- Some lens utility? Just trying this out
usesM :: MonadState s m => Getting a s a -> (a -> m r) -> m r
usesM lens f = do
    val <- use lens
    f val
{-# INLINE usesM #-}


(~>) :: MonadState s m => Getting a s a -> (a -> m r) -> m r
(~>) = usesM
{-# INLINE (~>) #-}

-- Utility for managing comparisons

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g x y = f (g x y)
infixr 9 .:

packWith :: (a -> b) -> a -> a -> (b, b)
packWith f x y = (f x, f y)

comp :: (a -> a -> Bool) -> (a, a) -> Bool
comp c (x, y) = c x y

intComp :: (Int32 -> Int32 -> Bool) -> Word32 -> Word32 -> Bool
intComp c = comp c .: packWith wordToInt

logic :: Bool -> Word32
logic False = 0
logic True  = 1

-- pretty print errors
formatErr :: (EmuError, VmInstruction, Word32) -> String
formatErr (e, inst, offset) =
    let formattedEmuError = case e of
            PopEmptyStack -> "Attempt to pop empty stack"
            DereferenceNull -> "Attempt to dereference null"
            ReadUninitializedMemory -> "Attempt to read uninitialized memory"
            InvalidPointerOffset off ->
                "Attempt to push to pointer offset " ++ show off
            PushDoubleFromPointer -> "Attempt to push double from pointer segment"
            PopDoubleToPointer -> "Attempt to pop double to pointer segment"
            NoSuchLabel label -> "Goto target does not exist: " ++ label
            UnalignedAddress addr -> "Misaligned memory address: " ++ show addr
            BehaviorNotImplemented bni -> "Behavior is not implemented: " ++ bni
        formattedInst = "At instruction: " ++ show inst
        formattedOffset = "At instruction offset: " ++ show offset
    in intercalate "\n" [formattedEmuError, formattedInst, formattedOffset]
