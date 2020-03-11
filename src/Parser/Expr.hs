{-# LANGUAGE ExistentialQuantification #-}
module Parser.Expr
    ( Expr(..)
    , parseExpr
    , parseRValue
    , gatherExpToRValue
    ) where

import Pretty
import TAC.Pretty

import Text.Parsec.Expr hiding (Operator)
import qualified Text.Parsec.Expr as Parsec
import Text.Parsec as TP hiding ((<|>), (<?>))
import Data.Text (pack, Text)

import Parser.Monad
import Compiler.Monad
import Compiler.SymbolTable
import TAC.Program

import Data.Function (on)
import Control.Monad (join, when, unless)
import Control.Monad.Debug

data Expr = Expr
    { exprGraph    :: Graph Insn O O
    , exprResult   :: TacExp
    , exprResultTy :: Type
    }

-- | ParsedExpr actions should /not/ do any parsing. They should be used for the
-- underlying CSpim parser actions, like 'mkFreshLocalTempUniq' or 'fail'.
-- The correct way to solve this problem would be to copy the code for Parsec's
-- buildExpressionParser, and allow monadic operator functions.
type ParsedExpr = Parser Expr

type Operator = Parsec.Operator Text ParserState Compiler ParsedExpr
type TermParser a = ParsecT Text ParserState Compiler a

parseExpr :: Parser Expr
parseExpr = join (P $ buildExpressionParser opTableAboveTernary under) <?> "expression"
  where under = pure <$> unP parseExprBelowTernary

parseExprAsRvalue :: Parser (Graph Insn O O, RValue, Type)
parseExprAsRvalue = do
    Expr g res ty <- parseExpr
    (g', rv) <- gather res ty
    return (g <*|*> g', rv, ty)

parseExprBelowTernary :: Parser Expr
parseExprBelowTernary = join $ P $ buildExpressionParser opTableBelowTernary term

term :: TermParser ParsedExpr
term = do
    rv <- unP parseRValue
    expr <- case rv of
        RVar (Left uniq)      -> identifierTerm uniq
        RVar (Right constant) -> constantTerm constant

    pure $ return expr
    -- pure $ return $ Expr emptyGraph (ValExp rv) $ IntTy Unsigned -- TODO: correct this type

identifierTerm :: Unique -> TermParser Expr
identifierTerm uniq = do
    mArgs <- TP.optionMaybe $ tpParens $ unP parseExprAsRvalue `TP.sepBy` unP comma
    case mArgs of
        Nothing ->   staticIdTerm uniq
        Just args -> funIdTerm uniq args

staticIdTerm :: Unique -> TermParser Expr
staticIdTerm uniq = do
    symTab <- _symTab <$> getState
    let Just ty = lookupVarType symTab uniq
    when (isFunTy ty) $ fail "CSpim doesn't support function pointers."
    return $ Expr emptyGraph (ValExp $ RVar $ Left uniq) ty

newtype ArgMonoid = AM { getAM :: (Graph Insn O O, [RValue]) }
instance Semigroup ArgMonoid where
    AM (g1, rvs1) <> AM (g2, rvs2) = AM (g1 <*|*> g2, rvs1 <> rvs2)
instance Monoid ArgMonoid where mempty = AM (emptyGraph, [])
funIdTerm :: Unique -> [(Graph Insn O O, RValue, Type)] -> TermParser Expr
funIdTerm uniq args = do
    symTab <- _symTab <$> getState
    let Just ty = lookupVarType symTab uniq
        Just funName = lookupVarName symTab uniq
        actArgTypes = map thd args
    unless (isFunTy ty) $ fail $ "Cannot call non-function `" <> funName <> "'"
    let FunTy retTy formalArgTypes = ty
    when (((/=) `on` length) formalArgTypes actArgTypes) $ unP $ customParseError
        $ FunctionCallBadlyTyped (pack funName) formalArgTypes actArgTypes

    resultTemp  <- unP $ mkFreshLocalTempUniq retTy
    returnLabel <- unP freshLabel
    let (argGraph, rvs) = getAM $ foldMap (\(g, r, _) -> AM (g, [r])) args
        callInst = Call uniq rvs returnLabel
        retInst  = Retrieve resultTemp
        graph = argGraph <*|*>  mkLast callInst
                         |*><*| mkLabel returnLabel
                         <*|*>  mkMiddle retInst

    return $ Expr graph (ValExp $ RVar $ Left resultTemp) retTy
  where thd (_, _, c) = c

constantTerm :: Constant -> TermParser Expr
constantTerm c = return $ Expr emptyGraph (ValExp $ RVar $ Right c) $ IntTy Unsigned

parseRValue :: Parser RValue
parseRValue = RVar <$> ((Left <$> (identifier >>= uniqueOf)) <|> (Right . IntConst <$> natural))

opTableAboveTernary :: [[Operator]]
opTableAboveTernary = [p14, p15]
  where p15 = []
        p14 = [ assignOp, addAssign, subAssign, timesAssign
              , divAssign, remAssign, shiftlAssign, shiftrAssign]

opTableBelowTernary :: [[Operator]]
opTableBelowTernary = [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12]
  where p1  = []
        p2  = []
        p3  = [timesOp, divOp, remOp]
        p4  = [addOp, subOp]
        p5  = [lshiftOp, rshiftOp]
        p6  = [ltOp, leqOp, gtOp, geqOp]
        p7  = [eqOp, neOp]
        p8  = []
        p9  = []
        p10 = []
        p11 = []
        p12 = []

timesOp, divOp, remOp :: Operator
timesOp = binary "*" Mul AssocLeft
divOp   = binary "/" Div AssocLeft
remOp   = binary "%" Rem AssocLeft

addOp, subOp :: Operator
addOp = binary "+" Add AssocLeft
subOp = binary "-" Sub AssocLeft

lshiftOp, rshiftOp :: Operator
lshiftOp = binary "<<" ShiftL AssocLeft
rshiftOp = binary ">>" ShiftR AssocLeft

ltOp, leqOp, gtOp, geqOp :: Operator
ltOp  = binary "<"  Lt AssocLeft
leqOp = binary "<=" Le AssocLeft
gtOp  = binary ">"  Gt AssocLeft
geqOp = binary ">=" Ge AssocLeft

eqOp, neOp :: Operator
eqOp = binary "==" Eq AssocLeft
neOp = binary "!=" Ne AssocLeft

assignOp, addAssign, subAssign, timesAssign      :: Operator
divAssign, remAssign, shiftlAssign, shiftrAssign :: Operator
assignOp = assign "=" Nothing
addAssign    = assign "+=" (Just Add)
subAssign    = assign "-=" (Just Sub)
timesAssign  = assign "*=" (Just Mul)
divAssign    = assign "/=" (Just Div)
remAssign    = assign "%=" (Just Rem)
shiftlAssign = assign "<<=" (Just ShiftL)
shiftrAssign = assign ">>=" (Just ShiftR)

binary :: String -> Binop -> Assoc -> Operator
binary name op = Infix parse
  where
    parse :: ParsecT Text ParserState Compiler (ParsedExpr -> ParsedExpr -> ParsedExpr)
    parse = unP $ reservedOp name >> connectBinop op

assign :: String -> Maybe Binop -> Operator
assign name op = Infix parse AssocRight
  where parse = unP $ reservedOp name >> connectAssign op

connectBinop :: Binop -> Parser (ParsedExpr -> ParsedExpr -> ParsedExpr)
connectBinop op = pure $ \e1 e2 -> do
    Expr lg lr lty <- e1
    Expr rg rr rty <- e2
    outTy <- commonRealType lty rty
    (gatherLeft, leftValue) <- gather lr lty
    (gatherRight, rightValue) <- gather rr rty
    return $ Expr (lg <*|*> gatherLeft <*|*> rg <*|*> gatherRight)
                  (Binop leftValue op rightValue)
                  outTy

connectAssign :: Maybe Binop -> Parser (ParsedExpr -> ParsedExpr -> ParsedExpr)
connectAssign mop = pure $ \e1 e2 -> do
    Expr lg lr lty <- e1
    case lr of
        ValExp _ -> pure ()
        _ -> do notLValue <- prettyTacExp lr
                fail $ notLValue ++ " is not an lvalue"
    Expr rg rr rty <- e2
    let outTy = lty
    (gatherLeft, leftValue) <- gather lr lty
    (gatherRight, rightValue) <- gather rr rty
    lvalue <- lvalueOfRValue leftValue
    let rhs = case mop of
            Nothing -> ValExp rightValue
            Just op -> Binop leftValue op rightValue
    return $ Expr (rg <*|*> gatherRight -- right-to-left to match gcc
                      <*|*> lg
                      <*|*> gatherLeft
                      <*|*> mkMiddle (lvalue := rhs))
                  (ValExp leftValue)
                  outTy
  where
    lvalueOfRValue :: RValue -> Parser LValue
    lvalueOfRValue (RVar (Left v)) = return $ LVar v
    lvalueOfRValue (RIxArr n v) = return $ LIxArr n v
    lvalueOfRValue r@(RVar (Right _)) = do
        notLValue <- prettyRValue r
        fail $ notLValue <> " is not an lvalue"

gather :: TacExp -> Type -> Parser (Graph Insn O O, RValue)
gather (ValExp rv) _ = return (emptyGraph, rv)
gather exp ty          = do
    temp <- mkFreshLocalTempUniq ty
    return (mkMiddle $ LVar temp := exp, RVar (Left temp))

gatherExpToRValue = gather

commonRealType :: Type -> Type -> Parser Type
commonRealType ty1 ty2
    | isIntegralTy ty1
    , isIntegralTy ty2 = pure $ makeSignageAgree (promote ty1) (promote ty2)
    | otherwise = panic "Parser.Expr.commonRealType: Non-integral types"

  where promote :: Type -> Type
        promote (IntTy Unsigned) = IntTy Unsigned
        promote _                = IntTy Signed

        makeSignageAgree :: Type -> Type -> Type
        makeSignageAgree ty1 ty2
          | IntTy Unsigned <- ty1 = IntTy Unsigned
          | IntTy Unsigned <- ty2 = IntTy Unsigned
          | otherwise             = IntTy Signed




--------------------------------------------------------------------------------------
--
-- Alternative to Nested Parser Strategy
--
--------------------------------------------------------------------------------------

{-
in theory, using this stuff, type ParsedExpr = ExprM Expr
instead of type ParseExpr = Parser Expr

would be cleaner and easier to handle (and probably faster as well). The complication
is that ExprM needs to be able to run Parser actions, because while creating an expression,
we can create new local variables.
-}

data ExprError prettyThing
     = ExprPanic Text
     | ExprFail  String
     | ExprPrettyFail prettyThing (prettyThing -> Parser String) -- f p >>= fail

exprError :: ExprError p -> Parser a
exprError (ExprPanic t) = panic t
exprError (ExprFail  t) = fail t
exprError (ExprPrettyFail p f) = f p >>= fail

data ExprM a = forall p. ExprM (Either (ExprError p) a)

runExprM :: ExprM a -> Parser a
runExprM (ExprM (Right a)) = pure a
runExprM (ExprM (Left e))  = exprError e

exprPanic :: Text -> ExprM a
exprFail  :: String -> ExprM a
exprPFail :: p -> (p -> Parser String) -> ExprM a
exprPanic t = ExprM $ Left (ExprPanic t)
exprFail  s = ExprM $ Left (ExprFail s)
exprPFail p f = ExprM $ Left (ExprPrettyFail p f)

exprMap f (ExprM expr) = ExprM $ fmap f expr

exprAp :: ExprM (a -> b) -> ExprM a -> ExprM b
exprAp  (ExprM (Right f)) (ExprM (Right a)) = ExprM $ Right $ f a
exprAp  (ExprM (Right _)) (ExprM (Left l))  = ExprM $ Left l
exprAp  (ExprM (Left l))  _                 = ExprM $ Left l

exprPure x = ExprM $ Right x
exprBind (ExprM (Right x)) f = f x
exprBind (ExprM (Left l))  _ = ExprM (Left l)

instance Functor ExprM where fmap = exprMap
instance Applicative ExprM where
    pure  = exprPure
    (<*>) = exprAp
instance Monad ExprM where (>>=) = exprBind
