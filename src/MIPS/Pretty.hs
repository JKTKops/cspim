{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module MIPS.Pretty {-(pretty)-} where

import qualified Pretty as P
import MIPS.Language
import MIPS.PrettyTH

import Data.Word
import Data.List
import qualified Data.Text as T

import Control.Monad.State
import Data.Functor (($>))

data Doc
     = DocString T.Text
     | DocCombine Doc Doc
     | DocIndent Int
     | DocNewline
     | DocEmpty
     deriving Show

render :: Doc -> T.Text
render d = evalState (renderDoc d) 0

renderDoc :: Doc -> State Int T.Text
renderDoc DocEmpty = pure ""
renderDoc (DocString s) = modify (+ T.length s) $> s
renderDoc (DocCombine d1 d2) = do
    s1 <- renderDoc d1
    s2 <- renderDoc d2
    return $ s1 <> s2
renderDoc (DocIndent indent) = do
    currentIndent <- get
    let numSpaces = case indent `compare` currentIndent of
            LT -> 1
            EQ -> 1  -- make one space so there's a gap
            GT -> indent - currentIndent
        spaces = T.replicate numSpaces " "
    put $ currentIndent + numSpaces
    return spaces
renderDoc DocNewline = put 0 $> "\n"

empty :: Doc
empty = DocEmpty

text :: T.Text -> Doc
text = DocString

char :: Char -> Doc
char c = DocString $ T.singleton c

indent :: Int -> Doc -> Doc
indent n d = DocIndent n <> d

(<+>) :: Doc -> Doc -> Doc
DocEmpty <+> d = d
d <+> DocEmpty = d
d1 <+> d2      = d1 <> DocString " " <> d2

parens :: Doc -> Doc
parens d = DocString "(" <> d <> DocString ")"

vsep :: [Doc] -> Doc
vsep = punctuate DocNewline

hsep :: [Doc] -> Doc
hsep = foldl (<+>) DocEmpty

punctuate :: Doc -> [Doc] -> Doc
punctuate p = mconcat . intersperse p

directiveList :: [Doc] -> Doc
directiveList = punctuate (text ", ")

instance Semigroup Doc where
    (<>) = DocCombine

instance Monoid Doc where
    mappend = (<>)
    mempty  = DocEmpty

class Pretty a where
    ppr :: a -> Doc
    pprList :: [a] -> Doc

    pprList xs = char '[' <> punctuate (char ',') (map ppr xs) <> char ']'

pretty :: Pretty a => a -> T.Text
pretty = render . ppr

instance Pretty a => Pretty [a] where
    ppr = pprList

instance Pretty Directive where
    ppr (DotByte bytes) = text ".byte" <+> directiveList (map ppr bytes)
    ppr (DotHalf hs)    = text ".half" <+> directiveList (map ppr hs)
    ppr (DotWord words) = text ".word" <+> directiveList (map ppr words)
    ppr (DotFloat fs)   = text ".float" <+> directiveList (map ppr fs)
    ppr (DotDouble ds)  = text ".double" <+> directiveList (map ppr ds)
    ppr (DotAscii str)  = text ".ascii" <+> ppr (show str) -- escapes and quotes
    ppr (DotAsciiz str) = text ".asciiz" <+> ppr (show str)
    ppr DotText         = text ".text"
    ppr DotData         = text ".data"
    ppr (DotGlobl str)  = text ".globl" <+> ppr str
    ppr (DotSetAt b)    = text ".set" <+> if b then text "at" else text "noat"

makeRegPrettyInstances

instance Pretty Char   where
    ppr c = char c
    pprList = text . T.pack

instance Pretty Word8  where ppr = ppr . show
instance Pretty Word16 where ppr = ppr . show
instance Pretty Word32 where ppr = ppr . show
instance Pretty Float  where ppr = ppr . show
instance Pretty Double where ppr = ppr . show
instance Pretty Offset where ppr = ppr . show
instance Pretty Src2 where
    ppr (Left reg)  = ppr reg
    ppr (Right imm) = ppr imm

instance Pretty Address where
    ppr (Left (label, Nothing))  = ppr label
    ppr (Left (label, Just off)) = ppr label <> char '+' <> ppr off
    ppr (Right (imm, reg))       = ppr imm <> parens (ppr reg)

makeMIPrettyInstance

instance Pretty MipsLine where
    ppr (ML (Just md) Nothing) = ppr md
    ppr (ML (Just md) (Just com)) = ppr md <> indent 40 (text "#" <+> ppr com)
    -- Perhaps we want to be able to note whether or not a comment should be indented?
    ppr (ML Nothing (Just com)) = text "#" <+> ppr com
    ppr (ML Nothing Nothing) = empty

    pprList ls = vsep (map ppr ls) <> text "\n"

instance Pretty MipsDeclaration where
    ppr (MDirective directive) = ppr directive
    ppr (MLabel label) = ppr label <> text ":"
    ppr (MInst mi) = indent 4 $ ppr mi

instance P.Pretty Reg             where pretty = MIPS.Pretty.pretty
instance P.Pretty FReg            where pretty = MIPS.Pretty.pretty
instance P.Pretty MipsLine        where
    pretty = MIPS.Pretty.pretty
    prettyList = render . pprList
instance P.Pretty MipsInstruction where pretty = MIPS.Pretty.pretty
instance P.Pretty MipsDeclaration where pretty = MIPS.Pretty.pretty
