{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module MIPS.Pretty (pretty) where

import qualified Pretty as P
import MIPS.Language
import MIPS.PrettyTH

import Data.Word
import Data.List

import Control.Monad.State
import Data.Functor (($>))

data Doc
     = DocString String
     | DocCombine Doc Doc
     | DocIndent Int
     | DocNewline
     | DocEmpty
     deriving Show

render :: Doc -> String
render d = evalState (renderDoc d) 0

renderDoc :: Doc -> State Int String
renderDoc DocEmpty = pure ""
renderDoc (DocString s) = modify (+ length s) $> s
renderDoc (DocCombine d1 d2) = do
    s1 <- renderDoc d1
    s2 <- renderDoc d2
    return $ s1 ++ s2
renderDoc (DocIndent indent) = do
    currentIndent <- get
    let numSpaces = case indent `compare` currentIndent of
            LT -> 1
            EQ -> 1  -- make one space so there's a gap
            GT -> indent - currentIndent
        spaces = replicate numSpaces ' '
    put $ currentIndent + numSpaces
    return spaces
renderDoc DocNewline = put 0 $> "\n"

empty :: Doc
empty = DocEmpty

text :: String -> Doc
text = DocString

char :: Char -> Doc
char c = DocString [c]

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

instance Semigroup Doc where
    (<>) = DocCombine

instance Monoid Doc where
    mappend = (<>)
    mempty  = DocEmpty

class Pretty a where
    ppr :: a -> Doc
    pprList :: [a] -> Doc

    pprList xs = char '[' <> punctuate (char ',') (map ppr xs) <> char ']'

pretty :: Pretty a => a -> String
pretty = render . ppr

instance Pretty a => Pretty [a] where
    ppr = pprList

instance Pretty Directive where
    ppr = text . show

makeRegPrettyInstances

instance Pretty Char   where
    ppr c = char c
    pprList = text

instance Pretty Word32 where ppr = text . show
instance Pretty Offset where ppr = text . show
instance Pretty Src2 where
    ppr (Left reg)  = ppr reg
    ppr (Right imm) = ppr imm

instance Pretty Address where
    ppr (Left label)       = ppr label
    ppr (Right (imm, reg)) = ppr imm <> parens (ppr reg)

makeMIPrettyInstance

instance Pretty MipsLine where
    ppr (ML (Just md) Nothing) = ppr md
    ppr (ML (Just md) (Just com)) = ppr md <> indent 40 (text "#" <+> text com)
    -- Perhaps we want to be able to note whether or not a comment should be indented?
    ppr (ML Nothing (Just com)) = text "#" <+> text com
    ppr (ML Nothing Nothing) = empty

    pprList = vsep . map ppr

instance Pretty MipsDeclaration where
    ppr (MDirective directive) = ppr directive
    ppr (MLabel label) = text label <> text ":"
    ppr (MInst mi) = indent 4 $ ppr mi

instance P.Pretty Reg             where pretty = MIPS.Pretty.pretty
instance P.Pretty FReg            where pretty = MIPS.Pretty.pretty
instance P.Pretty MipsLine        where pretty = MIPS.Pretty.pretty
instance P.Pretty MipsInstruction where pretty = MIPS.Pretty.pretty
instance P.Pretty MipsDeclaration where pretty = MIPS.Pretty.pretty
