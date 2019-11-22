{-# LANGUAGE DeriveDataTypeable, DeriveLift #-}
module MIPS.Language where

import Language.Haskell.TH.Syntax

import Data.Array
import Data.Data
import Data.Word
import Data.Int

type Label  = String
type Imm    = Int32
type Offset = Int32

-- TODO these directives are fucked and need arguments where appropriate
data Directive
     = DotByte   [Word8]
     | DotHalf   [Word16]
     | DotWord   [Word32]
     | DotFloat  [Float]
     | DotDouble [Double]
     | DotAscii  String
     | DotAsciiz String
     | DotText
     | DotData
     | DotGlobl  String
     deriving (Eq, Ord, Show, Typeable, Data, Lift)

data Reg
     = Reg0
     | RegAT -- ^ Use with extreme caution!
     | RegV0 | RegV1
     | RegA0 | RegA1 | RegA2 | RegA3
     | RegT0 | RegT1 | RegT2 | RegT3 | RegT4 | RegT5 | RegT6 | RegT7 | RegT8 | RegT9
     | RegS0 | RegS1 | RegS2 | RegS3 | RegS4 | RegS5 | RegS6 | RegS7
     | RegSP
     | RegRA
     | RegFP
     deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Lift)

-- | An association list of register indices to names, matching the 'Reg' data type.
regNumList :: [(Int, Reg)]
regNumList =
    [ (0, Reg0),  (2, RegV0), (3,  RegV1), (4,  RegA0), (5,  RegA1), (6,  RegA2), (7,  RegA3)
    , (8, RegT0), (9, RegT1), (10, RegT2), (11, RegT3), (12, RegT4), (13, RegT5), (14, RegT6)
    , (15, RegT7), (16, RegS0), (17, RegS1), (18, RegS2), (19, RegS3), (20, RegS4), (21, RegS5)
    , (22, RegS6), (23, RegS7), (24, RegT8), (25, RegT9), (29, RegSP), (31, RegRA)
    ]


data FReg
     = RegF0  | RegF1  | RegF2  | RegF3  | RegF4  | RegF5  | RegF6  | RegF7  | RegF8  | RegF9
     | RegF10 | RegF11 | RegF12 | RegF13 | RegF14 | RegF15 | RegF16 | RegF17 | RegF18 | RegF19
     | RegF20 | RegF21 | RegF22 | RegF23 | RegF24 | RegF25 | RegF26 | RegF27 | RegF28 | RegF29
     | RegF30 | RegF31
     deriving (Eq, Ord, Show, Typeable, Data, Lift)

fregNumList :: [(Int, FReg)]
fregNumList =
    [ (0, RegF0), (1, RegF1), (2, RegF2), (3, RegF3), (4, RegF4), (5, RegF5), (6, RegF6)
    , (7, RegF7), (8, RegF8), (9, RegF9), (10, RegF10), (11, RegF11), (12, RegF12)
    , (13, RegF13), (14, RegF14), (15, RegF15), (16, RegF16), (17, RegF17), (18, RegF18)
    , (19, RegF19), (20, RegF20), (21, RegF21), (22, RegF22), (23, RegF23), (24, RegF24)
    , (25, RegF25), (26, RegF26), (27, RegF27), (28, RegF28), (29, RegF29), (30, RegF30)
    , (31, RegF31)
    ]

type RDest = Reg
type RSrc  = Reg
type Src2  = Either Reg Imm

type FRDest = FReg
type FRSrc  = FReg

type Address = Either Label (Offset, Reg)

{-
This instruction set, as well as the arrangement and type synonym names,
taken from a MIPS instruction set reference which can be found at:
http://cs.wheatonma.edu/mgousie/comp220/SPIM_Quick_Reference.html
-}
data MipsInstruction
 -- | ARITHMETIC
 = MAbs   RDest RSrc

 | MAdd   RDest RSrc Src2
 | MAddu  RDest RSrc Src2
 | MSub   RDest RSrc Src2
 | MSubu  RDest RSrc Src2
   -- The following two instructions
   -- are 3 register divs.
   -- NOT 2 register hi/lo divs.
 | MDiv   RDest RSrc Src2
 | MDivu  RDest RSrc Src2
 | MMul   RDest RSrc Src2
 | MMulou RDest RSrc Src2 -- Try translating this as mulou
                          -- otherwise use multu .. mflo
 | MSll   RDest RSrc Src2
 | MSrl   RDest RSrc Src2
 | MSra   RDest RSrc Src2
 | MNeg   RDest RSrc
 | MNegu  RDest RSrc

 | MAnd   RDest RSrc Src2
 | MNor   RDest RSrc Src2
 | MOr    RDest RSrc Src2
 | MXor   RDest RSrc Src2
 | MNot   RDest RSrc      -- bitwise, use MSeq for logical

 | MSl    RDest RSrc Src2 -- signed and unsigned
 | MSr    RDest RSrc Src2 -- signed, sra
 | MSru   RDest RSrc Src2 -- unsigned, srl

 -- | Constant Manipulation
 | MLi    RDest Imm
 | MLui   RDest Imm

 -- | Comparisons

 | MSeq   RDest RSrc Src2
 | MSne   RDest RSrc Src2
 | MSge   RDest RSrc Src2
 | MSgeu  RDest RSrc Src2
 | MSgt   RDest RSrc Src2
 | MSgtu  RDest RSrc Src2
 | MSle   RDest RSrc Src2
 | MSleu  RDest RSrc Src2
 | MSlt   RDest RSrc Src2
 | MSltu  RDest RSrc Src2

 -- | Branches and Jumps
 | MB               Label -- branch
 | MBc1t            Label -- branch coproccesor 1 true
 | MBc1f            Label -- branch coprocceser 1 false
 | MBeq   RSrc Src2 Label -- branch equal
 | MBne   RSrc Src2 Label -- branch not equal
 | MBeqz  RSrc      Label -- branch equal to zero
 | MBnez  RSrc      Label -- branch not equal to zero
 | MBge   RSrc Src2 Label -- branch greater-or-equal
 | MBgeu  RSrc Src2 Label -- branch greater or equal unsigned
 | MBgez  RSrc      Label -- branch greater or equal to zero
 | MBgezal RSrc     Label -- above, and link
 | MBgt   RSrc Src2 Label -- branch greater than
 | MBgtu  RSrc Src2 Label -- branch greater than unsigned
 | MBgtz  RSrc      Label -- branch greater than zero
 | MBle   RSrc Src2 Label -- branch less or equal
 | MBleu  RSrc Src2 Label -- branch less or equal unsigned
 | MBlez  RSrc      Label -- branch less or equal to zero
 | MBltzal RSrc     Label -- above, and link
 | MBlt   RSrc Src2 Label -- branch less than
 | MBltu  RSrc Src2 Label -- branch less than unsigned
 | MBltz  RSrc      Label -- branch less than zero
 | MJ               Label -- jump
 | MJal             Label -- jump and link
 | MJalr  RSrc      Label -- jump and link register
 | MJr    RSrc            -- jump register

 -- | Load Instructions
 | MLa    RDest  Address
 | MLb    RDest  Address
 | MLbu   RDest  Address
 | MLd    RDest  Address -- WARNING: You probably meant to use MLD!
 | MLh    RDest  Address
 | MLhu   RDest  Address
 | MLw    RDest  Address
 | MLwc1  FRDest Address

 -- | Store Instructions
 | MSb    RSrc   Address
 | MSd    RSrc   Address -- use s.d to store from coprocessor 1
 | MSh    RSrc   Address
 | MSw    RSrc   Address

 -- | Movement Instructions
 | MMove  RDest RSrc
 | MMfhi  RDest        -- move from hi
 | MMflo  RDest        -- move from lo
 | MMthi  RDest        -- move to hi
 | MMtlo  RDest        -- move to lo
 | MMfc1  RDest FRSrc  -- move from coprocessor 1
 | MMtc1  RSrc  FRDest -- Move to coprocessor 1
 | MMfc1D RDest FRSrc  -- move double from coprocessor 1

 -- TODO Coprocessor 0 moves and registers

 -- | Floating Point Instructions
 | MAbsD  FRDest FRSrc | MAbsS  FRDest FRSrc
 | MNegD  FRDest FRSrc | MNegS  FRDest FRSrc

 | MAddD  FRDest FRSrc FRSrc | MAddS  FRDest FRSrc FRSrc
 | MSubD  FRDest FRSrc FRSrc | MSubS  FRDest FRSrc FRSrc
 | MMulD  FRDest FRSrc FRSrc | MMulS  FRDest FRSrc FRSrc
 | MDivD  FRDest FRSrc FRSrc | MDivS  FRDest FRSrc FRSrc

 | MCED  FRSrc FRSrc | MCES  FRSrc FRSrc
 | MCLeD  FRSrc FRSrc | MCLeS  FRSrc FRSrc
 | MCLtD  FRSrc FRSrc | MCLtS  FRSrc FRSrc

 -- these are conversion instructions.
 -- Converts from right type, to left type.
 -- ex: cvt.d.s converts a float to a double.
 | MCvtDS FRDest FRSrc | MCvtDW FRDest FRSrc
 | MCvtSD FRDest FRSrc | MCvtSW FRDest FRSrc
 | MCvtWD FRDest FRSrc | MCvtWS FRDest FRSrc

 | MLD FRDest Address | MLS FRDest Address
 | MSD FRDest Address | MSS FRDest Address

 | MMovD FRDest FRSrc | MMovS FRDest FRSrc

 -- | Exceptions and Traps
 | MRfe       -- return from exception
 | MSyscall   -- Call the system service in $v0
 | MBreak Imm -- cause exception <imm>
 | MNop       -- Do nothing.
 deriving (Eq, Ord, Show)

data MipsDeclaration
     = MDirective Directive
     | MLabel     Label
     | MInst      MipsInstruction
     deriving (Eq, Ord, Show)

data MipsLine = ML (Maybe MipsDeclaration) (Maybe String) -- declaration + comment
  deriving (Eq, Ord, Show)

-- declaration :: Lens' MipsLine (Maybe MipsDeclaration)
declaration :: Functor f
            => (Maybe MipsDeclaration -> f (Maybe MipsDeclaration))
            -> MipsLine -> f MipsLine
declaration f (ML decl comm) = (\decl' -> ML decl' comm) <$> f decl

-- comment :: Lens' MipsLine (Maybe String)
comment :: Functor f
        => (Maybe String -> f (Maybe String))
        -> MipsLine -> f MipsLine
comment f (ML decl comm) = ML decl <$> f comm


type Program = [MipsLine]
