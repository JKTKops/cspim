{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module VM.Language where

import Data.Array
import Data.Word

type Pointer = Word32
type Offset  = Word32

data InstType = InstByte
              | InstShort
              | InstUnsigned
              | InstSigned
              | InstFloat
              | InstDouble
  deriving (Eq, Ord, Enum, Bounded)

data VirtualMemorySegment
     = VmsStatic
     | VmsArg
     | VmsLocal
     | VmsPointer
     | VmsStruct
     | VmsArray
  deriving (Eq, Ord, Enum, Bounded)

-- An address is a memory segment + an offset
data VmAddress = VmAddress VirtualMemorySegment Offset
  deriving (Eq, Ord)

data VmInstruction
     -- should NOT push double immediates!
 = VmPush InstType (Either VmAddress Word32) -- A pure word is an immediate
 | VmPop  InstType VmAddress

   -- Control Flow constructs
 | VmLabel  String
 | VmGoto   String
 | VmIfGoto String
 | VmFunc   String Word32 Word32 -- function <name> <#args> <#locals>
 | VmReturn
 | VmCall   String Word32        -- call <name> <#args>

   -- TODO Conversion instructions

   -- TODO Bitwise instructions
 | VmComp -- complement

   -- Logical operations
 | VmAnd
 | VmOr
 | VmNor
 | VmXor
 | VmNot

   -- Signed comparisons
 | VmEq
 | VmNe
 | VmGe
 | VmGt
 | VmLe
 | VmLt

   -- Unsigned comparisons
   -- Eq/Ne use same instruction as signed
 | VmGeu
 | VmGtu
 | VmLeu
 | VmLtu

   {- Single-precision comparisons
      ne.s; ifgoto <label> becomes

      c.eq.s $fn $fm
      bc1f <label>       # note branch false
   -}
 | VmEqS
 | VmNeS
 | VmGeS
 | VmGtS
 | VmLeS
 | VmLtS

   -- Double-precision comparisons
 | VmEqD
 | VmNeD
 | VmGeD
 | VmGtD
 | VmLeD
 | VmLtD

   -- Signed integer operations
 | VmAdd
 | VmSub
 | VmMul
 | VmDiv
 | VmSll

   -- Unsigned operations
 | VmAddU
 | VmSubU
 | VmMulU
 | VmDivU

   -- Single-precision floating point operations
 | VmAddS
 | VmSubS
 | VmMulS
 | VmDivS

   -- Double-precision floating point operations
 | VmAddD
 | VmSubD
 | VmMulD
 | VmDivD

 | VmTerminate
 deriving (Eq, Ord)

isControlFlow :: VmInstruction -> Bool
isControlFlow inst = case inst of
    VmLabel _  -> True
    VmGoto _   -> True
    VmIfGoto _ -> True
    VmFunc _ _ _ -> True
    VmCall _ _ -> True
    VmReturn   -> True
    _          -> False

isLabel :: VmInstruction -> Bool
isLabel (VmLabel _) = True
isLabel _           = False

isLogicOp :: VmInstruction -> Bool
isLogicOp inst = case inst of
    VmAnd  -> True
    VmOr   -> True
    VmNor  -> True
    VmXor  -> True
    VmNot  -> True
    VmComp -> True
    _      -> False

isSignedComp :: VmInstruction -> Bool
isSignedComp inst = case inst of
    VmEq   -> True
    VmNe   -> True
    VmLt   -> True
    VmLe   -> True
    VmGt   -> True
    VmGe   -> True
    _      -> False

isUnsignedComp :: VmInstruction -> Bool
isUnsignedComp inst = case inst of
    VmLtu  -> True
    VmLeu  -> True
    VmGtu  -> True
    VmGeu  -> True
    _      -> False

isFloatComp :: VmInstruction -> Bool
isFloatComp inst = case inst of
    VmEqS  -> True
    VmNeS  -> True
    VmGtS  -> True
    VmGeS  -> True
    VmLtS  -> True
    VmLeS  -> True
    _      -> False

isDoubleComp :: VmInstruction -> Bool
isDoubleComp inst = case inst of
    VmEqD  -> True
    VmNeD  -> True
    VmGtD  -> True
    VmGeD  -> True
    VmLtD  -> True
    VmLeD  -> True
    _      -> False

isSignedOp :: VmInstruction -> Bool
isSignedOp inst = case inst of
    VmAdd  -> True
    VmSub  -> True
    VmMul  -> True
    VmDiv  -> True
    VmSll  -> True
    _      -> False

isUnsignedOp :: VmInstruction -> Bool
isUnsignedOp inst = case inst of
    VmAddU -> True
    VmSubU -> True
    VmMulU -> True
    VmDivU -> True
    _      -> False

isFloatOp :: VmInstruction -> Bool
isFloatOp inst = case inst of
    VmAddS -> True
    VmSubS -> True
    VmMulS -> True
    VmDivS -> True
    _      -> False

isDoubleOp :: VmInstruction -> Bool
isDoubleOp inst = case inst of
    VmAddD -> True
    VmSubD -> True
    VmMulD -> True
    VmDivD -> True
    _      -> False

pattern ControlFlow, LogicOp, SignedOp, UnsignedOp, FloatOp, DoubleOp :: VmInstruction
pattern ControlFlow  <- (isControlFlow  -> True)
pattern LogicOp      <- (isLogicOp      -> True)
pattern SignedComp   <- (isSignedComp   -> True)
pattern UnsignedComp <- (isUnsignedComp -> True)
pattern FloatComp    <- (isFloatComp    -> True)
pattern DoubleComp   <- (isDoubleComp   -> True)
pattern SignedOp     <- (isSignedOp     -> True)
pattern UnsignedOp   <- (isUnsignedOp   -> True)
pattern FloatOp      <- (isFloatOp      -> True)
pattern DoubleOp     <- (isDoubleOp     -> True)

type VmProgram = Array Word32 VmInstruction

data SystemCode
     = ScPrintInt | ScPrintFloat | ScPrintDouble | ScPrintString
     | ScReadInt  | ScReadFloat  | ScReadDouble  | ScReadString
     | ScSBRK     | ScExit
     deriving (Eq, Ord, Enum, Bounded)

instance Show InstType where
    show InstByte     = "byte"
    show InstShort    = "short"
    show InstUnsigned = "unsigned"
    show InstSigned   = "signed"
    show InstFloat    = "float"
    show InstDouble   = "double"

instance Show VirtualMemorySegment where
    show VmsStatic   = "static"
    show VmsArg      = "arg"
    show VmsLocal    = "local"
    show VmsPointer  = "pointer"
    show VmsStruct   = "struct"
    show VmsArray    = "array"

instance Show VmAddress where
    show (VmAddress segment int) = show segment ++ " " ++ show int

instance Show VmInstruction where
    show (VmPush insttype args) = "push " ++ show insttype ++ " " ++ case args of
        Left address -> show address
        Right int    -> show int
    show (VmPop insttype address) = "pop " ++ show insttype ++ " " ++ show address

    -- Control flow
    show (VmLabel  label) = "label "  ++ label ++ ":"
    show (VmGoto   label) = "goto "   ++ label
    show (VmIfGoto label) = "ifgoto " ++ label
    show (VmFunc label args lcls) = "function " ++ unwords [label, show args, show lcls]
    show (VmCall label args) = "call " ++ label ++ " " ++ show args
    show VmReturn            = "return"

    -- Logical operations
    show VmAnd  = "and"
    show VmOr   = "or"
    show VmNor  = "nor"
    show VmXor  = "xor"
    show VmNot  = "not"
    show VmComp = "comp"

   -- Signed comparisons
    show VmEq   = "eq"
    show VmNe   = "ne"
    show VmGe   = "ge"
    show VmGt   = "gt"
    show VmLe   = "le"
    show VmLt   = "lt"

   -- Unsigned comparisons
   -- Eq/Ne use same instruction as signed
    show VmGeu  = "geu"
    show VmGtu  = "gtu"
    show VmLeu  = "leu"
    show VmLtu  = "ltu"

   -- Single-precision comparisons
    show VmEqS  = "eq.s"
    show VmNeS  = "ne.s"
    show VmGeS  = "ge.s"
    show VmGtS  = "gt.s"
    show VmLeS  = "le.s"
    show VmLtS  = "lt.s"

   -- Double-precision comparisons
    show VmEqD  = "eq.d"
    show VmNeD  = "ne.d"
    show VmGeD  = "ge.d"
    show VmGtD  = "gt.d"
    show VmLeD  = "le.d"
    show VmLtD  = "lt.d"

    -- Signed integer operations
    show VmAdd  = "add"
    show VmSub  = "sub"
    show VmMul  = "mul"
    show VmDiv  = "div"

    show VmSll  = "sll"

    -- Unsigned operations
    show VmAddU = "addu"
    show VmSubU = "subu"
    show VmMulU = "mulu"
    show VmDivU = "divu"

    -- Single-precision floating point operations
    show VmAddS = "add.s"
    show VmSubS = "sub.s"
    show VmMulS = "mul.s"
    show VmDivS = "div.s"

    -- Double-precision floating point operations
    show VmAddD = "add.d"
    show VmSubD = "sub.d"
    show VmMulD = "mul.d"
    show VmDivD = "div.d"

    -- ends the program
    show VmTerminate = "terminate"

showProgram :: VmProgram -> String
showProgram = unlines . map show . elems

listProgram :: [VmInstruction] -> VmProgram
listProgram is = listArray (0, fromIntegral $ length is - 1) is
