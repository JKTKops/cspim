-------------------------------------------------------------------------------------------
-- QUASIQUOTER FOR TEMPLATE HASKELL
--
-- We want to be able to write literal MIPS code in the code generator. This is twofold:
-- 1) Simplify the process of reading and writing the MIPS translations of VM instructions.
-- 2) Easier to selectively add comments where desired.
-- Template Haskell is the way to go here: we need a parser, and [|mips|code] brackets
-- will be the EDSL.
--------------------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MIPS.Parser where

import MIPS.Language
import MIPS.ParserTH

import Data.List (foldl', intercalate)
import Data.Either (lefts, rights)
import Data.Functor (($>))
import Data.Word
import Data.Int

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Language
import Text.Parsec.Token hiding (natural, integer, parens)
import qualified Text.Parsec.Token as Parsec

import Control.Monad (guard)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Data.Data

type Parser a = Parsec String () a
-------------
-- parsec utils
-------------
tokParser = makeTokenParser haskellStyle

natural :: Parser Int
natural = fromIntegral <$> Parsec.natural tokParser

integer :: Integral a => Parser a
integer = fromIntegral <$> Parsec.integer tokParser

offset :: Parser Int32
offset = fromIntegral <$> Parsec.integer tokParser

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-------------
-- The 'Parsed' set of types. Includes a splice for generating ParsedMipsInstruction.
-------------

newtype ParsedImm     = PI (Either Imm Exp)                      deriving (Typeable, Data)
newtype ParsedOffset  = PO (Either Offset Exp)                   deriving (Typeable, Data)
newtype ParsedReg     = PR (Either Reg Exp)                      deriving (Typeable, Data)
newtype ParsedFReg    = PFR (Either FReg Exp)                    deriving (Typeable, Data)
type    ParsedRDest   = ParsedReg
type    ParsedRSrc    = ParsedReg
type    ParsedFRDest  = ParsedFReg
type    ParsedFRSrc   = ParsedFReg
newtype ParsedSrc2    = PS2 (Either ParsedReg ParsedImm)         deriving (Typeable, Data)
newtype ParsedLabel   = PL (Either Label Exp)                    deriving (Typeable, Data)
newtype ParsedAddress = PA (Either
                             (ParsedLabel, Maybe ParsedOffset)
                             (ParsedOffset, ParsedReg))          deriving (Typeable, Data)

data ParsedCommentPiece
     = StringPiece String
     | SplicePiece Exp
     deriving (Typeable, Data)
newtype ParsedComment = PC [ParsedCommentPiece]                  deriving (Typeable, Data)

instance Lift ParsedImm where
    lift (PI (Left imm))  = lift imm
    lift (PI (Right exp)) = return exp

instance Lift ParsedOffset where
    lift (PO (Left off))  = lift off
    lift (PO (Right exp)) = return exp

instance Lift ParsedReg where
    lift (PR (Left reg))  = lift reg
    lift (PR (Right exp)) = return exp

instance Lift ParsedFReg where
    lift (PFR (Left freg)) = lift freg
    lift (PFR (Right exp)) = return exp

instance Lift ParsedSrc2 where
    lift (PS2 inner) = lift inner

instance Lift ParsedLabel where
    lift (PL (Left lbl))  = lift lbl
    lift (PL (Right exp)) = return exp

instance Lift ParsedAddress where
    lift (PA inner) = lift inner

instance Lift ParsedComment where
    lift (PC parts) = do
        exps <- mapM liftPiece parts
        return $ catExps exps
      where liftPiece :: ParsedCommentPiece -> Q Exp
            liftPiece (StringPiece s) = return . LitE $ StringL s
            liftPiece (SplicePiece e) = return e

            catExps :: [Exp] -> Exp
            catExps [] = LitE $ StringL ""
            catExps (e:es) = UInfixE e (VarE '(++)) $ catExps es

-- Creates the types:
-- ParsedMipsInstruction
-- ParsedMipsDeclaration
makeParsedTypeDefinitions

-- Creates the instances:
-- Instance Lift ParsedMipsInstruction
-- Instance Lift ParsedMipsDeclaration
makePMILift

data ParsedMipsLine = PML (Maybe ParsedMipsDeclaration) (Maybe ParsedComment)

instance Lift ParsedMipsLine where
    lift (PML decl com) = apply 'ML [lift decl, lift com]

-------------
-- Actual Parsers!
-------------


parseReg :: Parser ParsedReg
parseReg = do
    char '$'
    PR <$> (Right <$> regSplice
       <|>  Left  <$> (regByNumber <|> regByName))
  where regSplice = spliceParser ""

regByNumber :: Parser Reg
regByNumber = do
    num <- natural
    let r = lookup num regNumList
    case r of
        Nothing  -> fail $ "regByNumber: register index " ++ show num ++ " is not allowed"
        Just reg -> return reg

regByName :: Parser Reg
regByName = regByNameZero <|> try regByNameATSPRAFP <|> regByNameNumber

regByNameZero, regByNameATSPRAFP, regByNameNumber :: Parser Reg
regByNameZero = (do string "zero"; return Reg0) <|> (do char '0'; return Reg0)

regByNameATSPRAFP = (string "at" $> RegAT)
                <|> (string "sp" $> RegSP)
                <|> (string "ra" $> RegRA)
                <|> (string "fp" $> RegFP)

regByNameNumber = do
    bank <- lower
    n <- natural
    let efr = case bank of
            'v' -> guard (allowedV n) >> lookup (n + 2) regNumList
            'a' -> guard (allowedA n) >> lookup (n + 4) regNumList
            't' -> guard (allowedT n) >> lookup (n2t n) regNumList
            's' -> guard (allowedS n) >> lookup (n + 16) regNumList
            _   -> Nothing
    case efr of
        Nothing  -> fail $ "Illegal register: $" ++ [bank] ++ show n
        Just reg -> return reg
  where allowedV n = n == 0 || n == 1
        allowedA n = n >= 0 && n <= 3
        allowedT n = n >= 0 && n <= 9
        allowedS n = n >= 0 && n <= 7
        n2t n = if n <= 7 then n + 8 else n + 16

-- | This parser is not truly sufficient for parsing MIPS directives.
--   Write them by hand when they need to be emitted.
parseDirective :: Parser Directive
parseDirective = foldl1 (<|>) $ map try
    [ string ".byte"   $> DotByte []
    , string ".half"   $> DotHalf []
    , string ".word"   $> DotWord []
    , string ".float"  $> DotFloat []
    , string ".double" $> DotDouble []
    , string ".ascii"  $> DotAscii ""
    , string ".asciiz" $> DotAsciiz ""
    , string ".text"   $> DotText
    , string ".data"   $> DotData
    , string ".globl"  $> DotGlobl ""
    , setat
    ]
  where setat :: Parser Directive
        setat = do
            string ".set"
            skipMany1 space
            on <- (string "at" $> True) <|> (string "noat" $> False)
            return $ DotSetAt on

parseFReg :: Parser ParsedFReg
parseFReg = do
    char '$'
    PFR <$> (Left <$> do
                    char 'f'
                    n <- natural
                    case lookup n fregNumList of
                        Nothing  -> fail $ "register $f" ++ show n ++ " does not exist"
                        Just reg -> return reg
        <|> Right <$> spliceParser "")

parseImm :: Parser ParsedImm
parseImm = PI <$> (Left <$> integer
              <|> Right <$> spliceParser "!")

parseOffset :: Parser ParsedOffset
parseOffset = PO <$> (Left <$> offset
                 <|> Right <$> spliceParser "!")

parseSrc2 :: Parser ParsedSrc2
parseSrc2 = PS2 <$> (Left <$> parseReg <|> Right <$> parseImm)

parseLabel :: Parser ParsedLabel
parseLabel = PL <$> (Left <$> do first <- letter
                                 rest  <- many1 $ alphaNum <|> char '_'
                                 return $ first : rest
                <|> (Right <$> spliceParser "@"))

parseAddr :: Parser ParsedAddress
parseAddr = PA <$> (labeledAddress
               <|> offsetAddress)

  where
    labeledAddress = Left <$> do
        label <- parseLabel
        mOff <- optionMaybe $ do
            try $ do skipMany space
                     char '+'
            skipMany space
            parseOffset
        return (label, mOff)

    offsetAddress = Right <$> do offset <- parseOffset
                                 reg    <- parens parseReg
                                 return (offset, reg)

spliceParser :: String -> Parser Exp
spliceParser prefix = do
    string prefix
    splice <- between (char '{') (char '}') $ many1 (noneOf "}")
    case parseExp splice of
        Left err  -> fail err
        Right exp -> return exp

-- Splice in a parser for MipsInstruction
-- defines:
-- parseMipsInstruction :: Parser MipsInstruction
makeMipsInstructionParser

parseMipsDecl :: Parser ParsedMipsDeclaration
parseMipsDecl =
        PMDirective <$> parseDirective
    <|> PMLabel     <$> try (parseLabel <* char ':')
    <|> PMInst      <$> parseMipsInstruction

parseComment :: Parser ParsedComment
parseComment = do
    try $ char '#' >> notFollowedBy (char 'N')
    PC <$> many1 parseCommentPiece <|> return (PC [])

parseCommentPiece :: Parser ParsedCommentPiece
parseCommentPiece = SplicePiece <$> try (spliceParser "#")
                <|> StringPiece <$> many1 (notFollowedBy (string "#{") *> anyChar)

parseMipsLine :: Parser ParsedMipsLine
parseMipsLine = do
    skipMany space
    decl <- optionMaybe parseMipsDecl
    skipMany space
    com <- optionMaybe parseComment
    skipMany anyChar
    return $ PML decl com

parseMipsProgList :: String -> Q (Either [String] [ParsedMipsLine])
parseMipsProgList prog = do
    (startLine, _) <- loc_start <$> location
    let mkSrcPos line = newPos "quasiquote" line 1
        prog_lines = zip (map mkSrcPos [startLine ..]) $ lines prog
        parseLine (pos, str) = parse (setPosition pos *> parseMipsLine <* eof) "quasiquote" str
        parseResults = map parseLine prog_lines
        parseErrors = map show $ lefts parseResults
    case parseErrors of
        []   -> return $ Right $ rights parseResults
        errs -> return $ Left errs

quoteMipsExp :: String -> Q Exp
quoteMipsExp s = parseMipsProgList s >>= \case
    Left errs -> do
        spliceLoc <- location
        let startPos = loc_start spliceLoc
            endPos     = loc_end spliceLoc
        fail $ "Error(s) while expanding quasiquoted MIPS at "
               ++ show startPos ++ "-" ++ show endPos ++ ":\n"
               ++ intercalate "\n\n" errs
    Right prog -> [| prog |]

mips :: QuasiQuoter
mips = QuasiQuoter
       { quoteExp  = quoteMipsExp
       , quotePat  = notHandled "Patterns"
       , quoteType = notHandled "Types"
       , quoteDec  = notHandled "Declarations"
       }
  where notHandled things = error $
          things ++ " are not handled by the mips quasiquoter."
