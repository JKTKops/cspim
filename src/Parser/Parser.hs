{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser.Parser (parseC) where

import TAC.Program as TAC hiding (NoChange, globalVars)
import qualified TAC.Program as Program
import MIPS.Language (Reg(..)) -- This is a complex problem because MIPS registers
                               -- being tied to the MemLoc type makes it harder to target x86
                               -- later. I think it might be possible to make MemLoc
                               -- parameterized over the _type_ of registers it contains.
import Pretty
import Compiler.Error
import Compiler.Flags
import Compiler.Monad
import Compiler.SymbolTable

import Parser.Monad
import Parser.Expr
import qualified Text.Parsec as P

import qualified Data.Map as M

import Data.DList
import Data.Functor (($>), (<&>))
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.State hiding (fail)
import Control.Applicative ((<|>))
import Control.Lens hiding (assign)
import Control.Lens.Utils

--------------------------------------------------------------------------------------
--
-- Core Parser
--
--------------------------------------------------------------------------------------

parseC :: FilePath -> Text -> Compiler Program
parseC fname src = crashOutOfScope parse
  where crashOutOfScope = handleC error warn ok
        error = compilerErrors
        warn es prog = isFlagSet FDeferOutOfScopeErrors <$> compilerFlags >>= \b ->
            if b then rethrowCErrs es $> prog
            else do
                modifyWarnings (reverse . flagAffects FDeferOutOfScopeErrors) (toList es)
                return prog

        -- DeferOutOfScopeErrors tells us to turn errors to warnings, but we have warnings
        -- and want to decide if we should turn them into errors.
        -- So we reverse the logic and reverse the adjustment.
        reverse E2Warning = W2Error
        reverse W2Error   = E2Warning -- future proofing

        ok = return

        parse = runParser startParser fname src

startParser :: Parser Program
startParser = mainFunction

mainFunction :: Parser Program
mainFunction = do
    reserved "int"
    name@"main" <- identifier
    main_uniq <- mkFreshGlobalUniq name FloatTy -- just a dummy type - it's a function!
    parens (pure ())
    entLbl <- labelFor "main"

    enterFunction main_uniq mapEmpty
    body <- block
    main_lcls <- exitFunction

    stackFrame <- buildStackFrame [] main_lcls

    let graph = mkFirst (Label entLbl)
                <*|*> mkMiddle (Enter main_uniq)
                <*|*> body
                <*|*> mkLast (Return main_uniq)
        fn = Fn { _name = main_uniq, _args = []
                , _locals = main_lcls, _stackFrame = stackFrame
                , _body = TacGraph graph entLbl
                }

    symTab.funcTable %= mapInsert main_uniq fn
    symTab ~> return . Prog [fn] [] []

block :: Parser (Graph Insn O O)
block = pushNewScope *> braces statementList <* popTopScope

statementList :: Parser (Graph Insn O O)
statementList = foldr (<*|*>) emptyGraph <$> many statement

statement :: Parser (Graph Insn O O)
statement = ((assign <|> retStmt <|> declare <|> pure emptyGraph) <* semi) <|> block

assign :: Parser (Graph Insn O O)
assign = do
    lname <- identifier >>= uniqueOf
    reservedOp "="
    Expr exprGraph finalTacExp _ <- parseExpr
    return $ exprGraph <*|*> mkMiddle (LVar lname := finalTacExp)

retStmt :: Parser (Graph Insn O O)
retStmt = do
    reserved "return"
    mrv <- optionMaybe parseRValue
    let setRv = case mrv of
            Nothing -> emptyGraph
            Just rv -> mkMiddle $ SetRV rv
    mfname <- use funName
    fname <- case mfname of
        Nothing -> P $ P.unexpected "return"
        Just n  -> pure n
    postLbl <- freshLabel
    return $ (setRv <*|*> mkLast (Return fname)) |*><*| mkLabel postLbl

declare :: Parser (Graph Insn O O)
declare = do
    reserved "int"
    name <- identifier
    uniq <- mkFreshLocalUniq name (IntTy Signed)
    m_g <- optionMaybe $ declaratorAssignment (LVar uniq)
    case m_g of
        Just graph -> pure graph
        Nothing    -> pure emptyGraph

declaratorAssignment :: LValue -> Parser (Graph Insn O O)
declaratorAssignment lval = do
    reservedOp "="
    Expr exprGraph finalTacExp _ <- parseExpr
    return $ exprGraph <*|*> mkMiddle (lval := finalTacExp)

-- Even if we should generate a TacExp here with the output of the final computation
-- we can't always do that because sometimes we need just the RValue (e.x. returning).
-- So we generate an extra temporary, and return an RVar with that temporary's unique.
-- The optimizer can eliminate it.
-- | Parse a C expression, such as what would appear on the right hand side of an assignment.
--   Returns an RValue with either the constant value or the unique of the variable
--   which will hold the final value of the expression.

parseTacExp :: Parser TacExp
parseTacExp = ValExp <$> parseRValue

--------------------------------------------------------------------------------------
--
-- Testing parsers
--
--------------------------------------------------------------------------------------

testParser :: Show a => Parser a -> Text -> IO String
testParser p src = case flip runCompiler noFlags $ runParser p "test" src of
   This errs -> mapM_ (T.putStrLn . pretty) errs >> return ""
   These errs a -> mapM_ (T.putStrLn . pretty) errs >> return (show a)
   That a -> return (show a)
