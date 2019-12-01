{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.Monad
    ( module Parser.Monad
    , (<|>)
    ) where

import Pretty

import TAC.Program hiding (NoChange, globalVars)
import MIPS.Language (Reg(..))
import Compiler.Error
import Compiler.Flags
import Compiler.Monad
import Compiler.SymbolTable

import Control.Monad.Fail (MonadFail)
import Control.Lens.TH

import Text.Parsec (ParsecT, ParseError, SourceName, runParserT, eof, getState, putState)
import qualified Text.Parsec as P
import qualified Parser.Lexer as L

import Data.Int
import qualified Data.Map as M
import qualified Data.Text as T
import Data.DList
import Data.Functor (($>), (<&>))
import Data.Maybe (isJust)

import Control.Monad.State hiding (fail)
import Control.Monad.Reader
import Control.Monad.Extra
import Control.Applicative (Alternative, (<|>))
import Control.Lens hiding (assign)
import Control.Lens.Utils

newtype Parser a = P { unP :: ParsecT
                                Text
                                ParserState
                                Compiler
                                a }
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadCompiler)

instance Pretty ParseError where pretty = T.pack . show

instance CompileError ParseError where
    flagAffects _ _ = NoChange

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
    }

makeLenses ''ParserState

instance MonadReader SymbolTable Parser where
    ask = gets _symTab

    local f p = do
        tab <- ask
        (symTab %= f) *> p <* (symTab .= tab)

runParser :: Parser a -> SourceName -> Text -> Compiler a
runParser p fname src = do
    res <- runParserT (L.whiteSpace *> unP p <* eof) emptyParserState fname src
    case res of
        Left err   -> compilerError err
        Right prog -> return prog

emptyParserState :: ParserState
emptyParserState = PS mapEmpty mapEmpty [] Nothing emptyTable []

data CustomParseError
     = VariableNotInScope P.SourcePos String

instance Pretty CustomParseError where
    pretty (VariableNotInScope p str) = T.pack $ show p <> ": Out of scope variable: `"
                                                        <> str <> "'"

instance CompileError CustomParseError where
    flagAffects FDeferOutOfScopeErrors (VariableNotInScope _ _) = E2Warning
    flagAffects _ _ = NoChange

customParseWarning :: CustomParseError -> Parser ()
customParseWarning = compilerWarning

customParseError :: CustomParseError -> Parser a
customParseError = fail . T.unpack . pretty

instance MonadState ParserState Parser where
    get = P getState
    put = P . putState

(<?>) :: Parser a -> String -> Parser a
(<?>) parser lbl = P (unP parser P.<?> lbl)

expecting :: String -> Parser a -> Parser a
expecting lbl p = p <?> lbl

try :: Parser a -> Parser a
try = P . P.try . unP

parens, braces, brackets, angles :: Parser a -> Parser a
parens   = P . P.between (L.symbol "(") (L.symbol ")") . unP
braces   = P . P.between (L.symbol "{") (L.symbol "}") . unP
brackets = P . P.between (L.symbol "[") (L.symbol "]") . unP
angles   = P . P.between (L.symbol "<") (L.symbol ">") . unP

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
-- Parser state utils
--
--------------------------------------------------------------------------------------

-- | Enter a new lexical scope.
pushNewScope :: Parser ()
pushNewScope = localVarScope %= (mapEmpty:)

-- | Exit a lexical scope. Returns a map of all the /user-defined/
--   variables created inside that scope. Compiler temporaries are not in the map!
--   (What would their names be?)
popTopScope :: Parser NameMap
popTopScope = do
    scopes <- use localVarScope
    case scopes of
        [] -> panic "Attempt to pop variable scope from empty stack"
        (x:xs) -> (localVarScope %= tail) $> x

-- | Manages the steps of entering a function definition, including pushing a scope
--   and putting the arguments of the function (the NameMap) into it.
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
--   Be careful to avoid name capture when using this function, or use 'prefixedFreshLabel'
--   instead.
--
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
--   produced by this function.
--
--   The label's name is of the form: $<given string><num> where the number is guaranteed
--   to be distinct from other numbers produced by this function. The `$` should prevent
--   accidental name capture by users.
prefixedFreshLabel :: String -> Parser Label
prefixedFreshLabel name = do
    lbl <- freshLabel
    let lbl_name = "$" ++ name ++ drop 1 (show lbl)
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
            pos <- P P.getPosition
            customParseWarning $ VariableNotInScope pos name
            freshUnique

searchLocalVarStack :: String -> [NameMap] -> Maybe Unique
searchLocalVarStack _ [] = Nothing
searchLocalVarStack name (m:ms) = case mapLookup name m of
    Just uniq -> Just uniq
    Nothing   -> searchLocalVarStack name ms

-- | Makes a fresh unique for a local variable. Adds it to the '_funLocals' table.
--   If a ('String') name is given, additionally adds that name to the local variable
--   scope and to the symbol table.
--
--   Panics if there is no local scope.
mkFreshLocalUniqMaybe :: Maybe String -> Type -> Parser Unique
mkFreshLocalUniqMaybe mname vtype = do
    uniq <- freshUnique
    whenM (localVarScope `hasn'tM` _head) $ panic "Parser.mkFreshLocalUniq: No local scope!"
    whenJust mname $ \name -> localVarScope._head %= mapInsert name uniq
    funLocals %= (uniq:)
    symTabAddVar uniq mname vtype
    return uniq

-- | Creates a new unique for the given name and assigns it in the innermost local scope.
--   This function will panic if there is no local scope!
mkFreshLocalUniq :: String -> Type -> Parser Unique
mkFreshLocalUniq str = mkFreshLocalUniqMaybe (Just str)

-- | Creates a new unique for a temporary variable, such as for an intermediate result
--   in an expression.
--
--   Panics if there is no local scope.
mkFreshLocalTempUniq :: Type -> Parser Unique
mkFreshLocalTempUniq = mkFreshLocalUniqMaybe Nothing

-- | Creates a new unique for the given name and assigns it in the global scope.
mkFreshGlobalUniq :: String -> Type -> Parser Unique
mkFreshGlobalUniq name vtype = do
    uniq <- freshUnique
    globalVars %= mapInsert name uniq
    symTabAddVar uniq (Just name) vtype
    return uniq

symTabAddVar :: Unique -> Maybe String -> Type -> Parser ()
symTabAddVar uniq mname vtype = do
    whenJust mname $ \name -> symTab.varNames %= mapInsert uniq name
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
    let addr = alignOffset current $ fromIntegral $ sizeof ty
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
                                $ "Unique " <> T.pack (show var)
                                  <> " has not been assigned a type!"
                                  <> " (while allocating arguments)"
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

