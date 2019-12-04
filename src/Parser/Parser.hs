{-# LANGUAGE FlexibleContexts #-}
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
import Parser.Type
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
                -- main always returns 0 if it reaches the end
                <*|*> mkLast (Return $ Just $ RVar $ Right $ IntConst 0)
        fn = Fn { _name = main_uniq, _args = []
                , _locals = main_lcls, _stackFrame = stackFrame
                , _body = TacGraph graph entLbl
                }

    symTab.funcTable %= mapInsert main_uniq fn
    symTab ~> return . Prog [fn] [] []

block :: Parser (Graph Insn O O)
block = pushNewScope *> braces blockItemList <* popTopScope

blockItem :: Parser (Graph Insn O O)
blockItem = statement <|> declare

blockItemList :: Parser (Graph Insn O O)
blockItemList = foldr (<*|*>) emptyGraph <$> many blockItem

statement :: Parser (Graph Insn O O)
statement = expecting "statement" $
            labeledStatement
        <|> ((exprStmt <|> retStmt <|> gotoStmt) <* semi)
        <|> (semi $> emptyGraph)
        <|> block
-- TODO NEXT: re-combine Return and SetRV, have code generator generate a return label at the
-- end of each function and put the returning code there
-- i.e. assign $v0; goto $L15; $L15: ... # start return sequence
-- A mips optimizer in the future could do small-block catentation if desired.

-- | Parse a labeled statement.
--   TODO: Labels include switch statement `case` and `default` labels.
labeledStatement :: Parser (Graph Insn O O)
labeledStatement = do
    lblName <- try $ identifier <* reservedOp ":" <?> "label"
    lbl <- labelFor lblName
    stmtG <- statement
    let graph = mkBranch lbl |*><*| mkLabel lbl <*|*> stmtG
    return graph

-- | Parse an expression and retrieve the graph, throwing away the resulting TacExp
--   and its type.
exprStmt :: Parser (Graph Insn O O)
exprStmt = do
    Expr exprGraph _ _ <- parseExpr
    return exprGraph

-- | Parse a return statement.
--
--   Return instructions implicitly jump to a return label which is created by the code
--   generator. If the MIPS optimizer decides it is worthwhile to duplicate that code,
--   then small block catenation will duplicate it later. We don't concern ourselves with that
--   here.
retStmt :: Parser (Graph Insn O O)
retStmt = do
    reserved "return"
    mrv <- optionMaybe parseRValue
    let retInst = mkLast $ Return mrv
    postLbl <- freshLabel
    return $ retInst |*><*| mkLabel postLbl

-- | Parse a simple declaration, possibly with an initializer.
--   TODO: switch to type specifier/declaration specifier model;
--   entry point for function definitions
declare :: Parser (Graph Insn O O)
declare = do
    ty   <- parseType
    name <- identifier
    uniq <- mkFreshLocalUniq name ty
    m_g <- optionMaybe $ declaratorAssignment (LVar uniq)
    case m_g of
        Just graph -> pure graph
        Nothing    -> pure emptyGraph

declaratorAssignment :: LValue -> Parser (Graph Insn O O)
declaratorAssignment lval = do
    reservedOp "="
    Expr exprGraph finalTacExp _ <- parseExpr
    return $ exprGraph <*|*> mkMiddle (lval := finalTacExp)

gotoStmt :: Parser (Graph Insn O O)
gotoStmt = do
    reserved "goto"
    lblName <- identifier
    lbl <- labelFor lblName
    fakeLbl <- freshLabel
    return $ mkBranch lbl |*><*| mkLabel fakeLbl

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
testParser p src = do
    result <- testCompilerIO (runParser p "test" src) noFlags
    case result of
        This errs -> mapM_ (T.putStrLn . pretty) errs >> return ""
        These errs a -> mapM_ (T.putStrLn . pretty) errs >> return (show a)
        That a -> return (show a)
