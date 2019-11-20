{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
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

import qualified Parser.Lexer as L
import Text.Parsec (between, getState, putState, ParsecT, ParseError)
import qualified Text.Parsec as P

import qualified Data.Map as M

import Data.DList
import Data.Functor (($>), (<&>))
import Data.Int
import Data.Maybe (isJust)

import Control.Monad.State hiding (fail)
import Control.Monad.Fail (MonadFail)
import Control.Applicative (Alternative, (<|>))
import Control.Lens.TH
import Control.Lens hiding (assign)
import Control.Lens.Utils

newtype Parser a = P { unP :: ParsecT
                                String
                                ParserState
                                Compiler
                                a }
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadCompiler)

instance Pretty ParseError where pretty = show

instance CompileError ParseError where
    flagAffects _ _ = NoChange

instance UniqueMonad Parser where
    freshUnique = state $ \ps -> let (u:us) = _freshUniques ps
                                 in (u, ps { _freshUniques = us })

type NameMap = M.Map String Unique

data ParserState = PS
    { _globalVars    :: NameMap -- function names go here as they are created
                                -- TODO:
                                -- At some point it might be a good idea to also put them in
                                -- the varType map to catch errors with argument counts etc
    , _labels        :: M.Map String Label -- making LabelMap is ambiguous
    , _localVarScope :: [NameMap]
    , _funName       :: Maybe Unique
    , _symTab        :: SymbolTable
    , _funLocals     :: [Unique]           -- all the local variables created in this function

    , _freshUniques  :: [Unique]
    }

makeLenses ''ParserState

runParser :: Parser a -> P.SourceName -> String -> Compiler a
runParser p fname src = do
    res <- P.runParserT (L.whiteSpace *> unP p <* P.eof) emptyParserState fname src
    case res of
        Left err   -> compilerError err
        Right prog -> return prog

emptyParserState :: ParserState
emptyParserState = PS mapEmpty mapEmpty [] Nothing emptyTable [] [1..]

parens, braces, brackets, angles :: Parser a -> Parser a
parens   = P . between (L.symbol "(") (L.symbol ")") . unP
braces   = P . between (L.symbol "{") (L.symbol "}") . unP
brackets = P . between (L.symbol "[") (L.symbol "]") . unP
angles   = P . between (L.symbol "<") (L.symbol ">") . unP

symbol :: String -> Parser String
symbol = P . L.symbol

natural :: Parser Integer
natural = P L.natural

float :: Parser Double
float = P L.float

dot, semi :: Parser ()
dot = P L.dot
semi = P L.semi

reserved :: String -> Parser ()
reserved = P . L.reserved

identifier :: Parser String
identifier = P L.identifier

reservedOp :: String -> Parser ()
reservedOp = P . L.reservedOp

many, many1 :: Parser a -> Parser [a]
many = P . P.many . unP
many1 = P . P.many1 . unP

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = P . P.optionMaybe . unP

--------------------------------------------------------------------------------------
--
-- Core Parser
--
--------------------------------------------------------------------------------------

parseC :: String -> String -> Compiler Program
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
    rval <- parseRValue
    return . mkMiddle $ LVar lname := rval

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
    rval <- parseRValue
    return . mkMiddle $ lval := rval

parseRValue :: Parser RValue
parseRValue = RVar <$> ((Left <$> (identifier >>= uniqueOf)) <|> (Right . IntConst <$> natural))

--------------------------------------------------------------------------------------
--
-- Parser state utils
--
--------------------------------------------------------------------------------------

data CustomParseError
     = VariableNotInScope String

instance Pretty CustomParseError where
    pretty (VariableNotInScope str) = "Out of scope variable: `" ++ str ++ "'"

instance CompileError CustomParseError where
    flagAffects FDeferOutOfScopeErrors (VariableNotInScope _) = E2Warning
    flagAffects _ _ = NoChange

customParseWarning :: CustomParseError -> Parser ()
customParseWarning = compilerWarning

customParseError :: CustomParseError -> Parser a
customParseError = compilerError

instance MonadState ParserState Parser where
    get = P getState
    put = P . putState

pushNewScope :: Parser ()
pushNewScope = localVarScope %= (mapEmpty:)

popTopScope :: Parser NameMap
popTopScope = do
    scopes <- use localVarScope
    case scopes of
        [] -> panic "Attempt to pop variable scope from empty stack"
        (x:xs) -> (localVarScope %= tail) $> x

enterFunction :: Name -> NameMap -> Parser ()
enterFunction n as = do
    pushNewScope
    localVarScope._head .= as
    funName ?= n

-- | Exits the definition of the current function.
--   Returns a list of all the local variables defined in the function.
exitFunction :: Parser [Name]
exitFunction = do
    popTopScope
    funName   .= Nothing
    fmap reverse $ funLocals <<.= []

-- | Searches for a label with the given name. If none exist, creates a new one.
--   The created label is guaranteed to be unique, but the name associated with it is not.
--   Be careful to avoid name capture when using this function, or use 'freshLabelFor' instead.
--   This function /should/ be used to get labels for function names,
--   as those must be capturable.
labelFor :: String -> Parser Label
labelFor name = do
    m_lbl <- uses labels $ mapLookup name
    case m_lbl of
        Just lbl -> pure lbl
        Nothing  -> do
            lbl <- freshLabel
            labels %= mapInsert name lbl
            symTab.labelNames %= mapInsert lbl name
            pure lbl

-- | Produces a fresh label that is guaranteed to be distinct from any other label
--   produced by this function. The given string will be the prefix of the label's
--   associated name.
prefixedFreshLabel :: String -> Parser Label
prefixedFreshLabel name = do
    lbl <- freshLabel
    let lbl_name = name ++ drop 1 (show lbl)
    labels %= mapInsert lbl_name lbl
    symTab.labelNames %= mapInsert lbl lbl_name
    pure lbl

-- | Searches for the unique for a given name available in the current scope.
--   This function will panic if there is no such unique!
uniqueOf :: String -> Parser Unique
uniqueOf name = do
    m1 <- uses localVarScope $ searchLocalVarStack name
    m2 <- uses globalVars $ mapLookup name
    let m_uniq = m1 <|> m2
    case m_uniq of
        Just uniq -> return uniq
        Nothing   -> do
            customParseWarning $ VariableNotInScope name
            freshUnique

searchLocalVarStack :: String -> [NameMap] -> Maybe Unique
searchLocalVarStack _ [] = Nothing
searchLocalVarStack name (m:ms) = case mapLookup name m of
    Just uniq -> Just uniq
    Nothing   -> searchLocalVarStack name ms

-- | Creates a new unique for the given name and assigns it in the innermost local scope.
--   This function will panic if there is no local scope!
mkFreshLocalUniq :: String -> Type -> Parser Unique
mkFreshLocalUniq name vtype = do
    uniq <- freshUnique
    whenM (localVarScope `hasn'tM` _head) $ panic "Parser.mkFreshLocalUniq: No local scope!"
    localVarScope._head %= mapInsert name uniq
    funLocals %= (uniq:)
    symTabAddVar uniq name vtype
    return uniq

-- | Creates a new unique for the given name and assigns it in the global scope.
mkFreshGlobalUniq :: String -> Type -> Parser Unique
mkFreshGlobalUniq name vtype = do
    uniq <- freshUnique
    globalVars %= mapInsert name uniq
    symTabAddVar uniq name vtype
    return uniq

symTabAddVar :: Unique -> String -> Type -> Parser ()
symTabAddVar uniq name vtype = do
    symTab.varNames %= mapInsert uniq name
    symTab.varTypes %= mapInsert uniq vtype

--------------------------------------------------------------------------------------
--
-- Computing allocations for variables
--
--------------------------------------------------------------------------------------

-- NOTE: we need access to the type tables here so all wrapped up in Parser

newtype AllocT m a = AT { unAT :: StateT Int32 m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

runAllocT :: AllocT m a -> m (a, Int32)
runAllocT (AT action) = runStateT action 0

addrFor :: Monad m => Type -> AllocT m Int32
addrFor ty = (AT $ do
    current <- get
    let addr = alignOffset current 4
    state $ const (addr, addr)) <* consumeType ty

consumeSize :: Monad m => Int -> AllocT m ()
consumeSize n = AT $ modify $ \s -> s + fromIntegral n

consumeType :: Monad m => Type -> AllocT m ()
consumeType = consumeSize . sizeof

-- | Computes the allocation table for a list of local variables. Everything is allocated
--   to the stack. Returns an allocation table that is based at offset 0, and the
--   total stack space consumed by the allocation.
computeLocalVarAllocation :: [Name] -> Parser (UniqueMap MemLoc, Int32)
computeLocalVarAllocation vars = do
    (offsets, totalSpace) <- runAllocT $ computeAllocs vars
    let lclAllocTable = mapFromList $ zip vars (map OffsetLoc offsets)
    return (lclAllocTable, totalSpace)

{- |
Computes the allocation table for a list of function arguments.

The input list should be in the left-to-right order of the function's parameters.

Returns a triple of register allocated variables, stack allocated variables, and the total
stack words consumed.
-}
computeArgumentVarAllocation :: [Name] -> Parser ([(Name, MemLoc)], [(Name, MemLoc)], Int32)
computeArgumentVarAllocation vars = do
    let (regArgs, stackArgs) = splitAt 4 vars -- this is wrong! TODO TODO TODO TODO TODO
        -- updated TODO is to abstract this out into a memory model class
        regLocs = zip regArgs $ map RegLoc [RegA0 .. RegA3]
    (offsets, totalSpace) <- runAllocT $ computeAllocs stackArgs
    let stackLocs = zip stackArgs $ map OffsetLoc offsets
    return (regLocs, stackLocs, totalSpace)

computeArgumentPassingTable :: [Name] -> Parser (UniqueMap MemLoc, Int32)
computeArgumentPassingTable vars = do
    (regs, stacks, size) <- computeArgumentVarAllocation vars
    return (mapFromList regs `mapUnion` mapFromList stacks, size)

computeAllocs :: [Name] -> AllocT Parser [Int32]
computeAllocs vars = sequence $ computeAlloc <$> vars
  where computeAlloc var = do
            m_varTy <- lift $ uses (symTab.varTypes) $ mapLookup var
            varTy <- case m_varTy of
                Nothing -> lift $ panic
                                $ "Unique " ++ show var ++ " has not been assigned a type!"
                               ++ " (while allocating arguments)"
                Just varTy -> pure varTy
            addrFor varTy

-- | Takes a list of arguments and a list of local variables. Allocates space for both,
--   placing the allocations into the symbol table. Constructs a stack frame that represents
--   the size of the frame and locations of saved registers.
buildStackFrame :: [Name] -> [Name] -> Parser StackFrame
buildStackFrame args lcls = do
    (passRegs, passStacks, argSize) <- computeArgumentVarAllocation args
    (lclMap, lclSize) <- computeLocalVarAllocation $ lcls ++ map fst passRegs
    let endLcl = alignOffset lclSize 4
        savedRegs = mapFromList [(RegRA, OffsetLoc endLcl), (RegFP, OffsetLoc $ endLcl + 4)]
        endSaved = endLcl + 8
        argMap = adjustOffset endSaved <$> mapFromList passStacks
        passingTable = mapFromList passRegs `mapUnion` argMap
    symTab.allocTable %= \m -> lclMap `mapUnion` argMap `mapUnion` m
    -- the size is endSaved because that's how much
    -- space needs to be allocated for the frame.
    return $ StackFrame endSaved savedRegs passingTable
  where adjustOffset a (OffsetLoc o) = OffsetLoc $ a + o
        adjustOffset _ loc = loc

--------------------------------------------------------------------------------------
--
-- Testing parsers
--
--------------------------------------------------------------------------------------

testParser :: Show a => Parser a -> String -> IO String
testParser p src = case flip runCompiler noFlags $ runParser p "test" src of
   This errs -> mapM_ (putStrLn . pretty) errs >> return ""
   These errs a -> mapM_ (putStrLn . pretty) errs >> return (show a)
   That a -> return (show a)
