{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Parser.Lexer where

import Text.Parsec as P
import Text.Parsec.Char
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

import Data.Char
import Data.Text
import Data.Function ((&))
import Data.Functor  (($>))

import Control.Monad (when)

type Lexer = ParsecT Text

lexeme :: Monad m => Lexer u m a -> Lexer u m a
lexeme l = try l <* whiteSpace

symbol :: Monad m => String -> Lexer u m String
symbol s = lexeme (string s) <?> "`" ++ s ++ "'"

natural :: (Monad m, Num a) => Lexer u m a
natural = lexeme nat <?> "natural"

float :: Monad m => Lexer u m Double
float = lexeme flt <?> "float"

dot, comma, semi :: Monad m => Lexer u m ()
dot   = lexeme (char '.' $> ()) <?> "`.'"
comma = lexeme (char ',' $> ()) <?> "`,'"
semi  = lexeme (char ';' $> ()) <?> "`;'"

reserved :: Monad m => String -> Lexer u m ()
reserved s = lexeme (string s *> notFollowedBy alphaNum) <?> "`" ++ s ++ "'"

reservedOp :: Monad m => String -> Lexer u m ()
reservedOp s = lexeme (string s <* notFollowedBy (T.opLetter cStyle)) $> () <?> "`" ++ s ++ "'"

identifier :: Monad m => Lexer u m String
identifier = lexeme (p >>= check) <?> "identifier"
  where
    p = (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')
    check x = if x `elem` cReservedWords
                 then fail $ "keyword `" ++ x ++ "' cannot be an identifier."
                 else return x

charLiteral :: Monad m => Lexer u m Char
charLiteral = lexeme litChar <?> "character literal"

stringLiteral :: Monad m => Lexer u m String
stringLiteral = lexeme litString <?> "string literal"

whiteSpace :: Monad m => Lexer u m ()
whiteSpace = skipMany (simpleSpace
                          <|> oneLineComment
                          <|> multiLineComment
                          <|> cppSourcePosLine
                          <?> "")

  where
    simpleSpace = skipMany1 (satisfy isSpace)

    oneLineComment = do
        try $ string "//"
        skipMany $ satisfy (/= '\n')
    multiLineComment = do
        try $ string "/*"
        inComment
        return ()
      where inComment = try (string end)
                    <|> skipMany1 (noneOf end) *> inComment
                    <|> oneOf end *> inComment
            end = "*/"

    -- Nothing too spectacular going on here. CPP position markers have the form:
    -- # <num> "filename" flags
    -- where <num> is the line number of the following line and we can ignore the flags.
    cppSourcePosLine = do
        guardUnindented
        string "# "
        num <- nat
        filename <- parseFilename
        skipMany $ satisfy (/= '\n')
        pos <- getPosition
        setPosition $ pos & flip setSourceName filename & flip setSourceLine (num - 1)

    guardUnindented = getPosition >>= \pos ->
        when (sourceColumn pos > 1) parserZero

    -- hacky string parser that doesn't consume whitespace and is sufficient for filenames
    parseFilename :: Monad m => Lexer u m String
    parseFilename = char '"' *> many (satisfy (/= '"')) <* char '"'


cReservedWords =
    [ -- control flow
      "if", "else" , "do", "while", "for"
    , "break", "continue", "goto"
    , "switch", "case", "default"
    , "return"

    -- enabling external/global names
    , "extern", "static"

    -- The fabled register keyword
    , "register"

    -- Compile-time keywords
    , "sizeof", "alignof"

    -- types
    , "char", "short", "int", "long"
    , "float", "double", "void", "unsigned"
    , "typedef", "struct", "union", "enum"
    ]
cOps =
    [ "+", "*", "/", "%", "-", "<<", ">>"
    , "&", "&&", "|", "||", "^", "~", "!"
    , "++", "--"
    , ">", ">=", "<=", "<", "==", "!="
    , "=", "+=", "-=", "*=", "/=", "%="
    , ">>=", "<<=", "&=", "|=", "^="
    ]

--------------------------------------------------------------------------------------
--
-- A C-style and several extracted lexers. DO NOT USE OTHER LEXERS FROM THIS STYLE.
--
-- These are internal to this module for the purposes of writing our own lexer, which
-- uses its own space consumer.
--
--------------------------------------------------------------------------------------


cStyle :: Monad m => T.GenLanguageDef Text st m
cStyle = T.LanguageDef
  { T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = "//"
  , T.nestedComments  = False
  , T.identStart      = P.letter
  , T.identLetter     = P.alphaNum <|> P.oneOf "_'"
  , T.opStart         = T.opLetter cStyle
  , T.opLetter        = P.oneOf "+-*/%<>!~^=&|"
  , T.reservedNames   = cReservedWords
  , T.reservedOpNames = cOps
  , T.caseSensitive   = True
  }

tokP :: Monad m => T.GenTokenParser Text st m
tokP = T.makeTokenParser cStyle

-- do NOT use T.integer - it will parse spaces between the sign and the number, but we
-- need to use our own space consumer!
nat :: (Monad m, Num a) => Lexer u m a
nat = fromInteger <$> T.natural tokP

flt :: Monad m => Lexer u m Double
flt = T.float tokP

litChar :: Monad m => Lexer u m Char
litChar = T.charLiteral tokP

litString :: Monad m => Lexer u m String
litString = T.stringLiteral tokP
