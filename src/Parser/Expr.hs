module Parser.Expr
    ( Expr(..)
    , parseExpr
    , parseRValue
    ) where

import Pretty
import TAC.Pretty

import Text.Parsec.Expr hiding (Operator)
import qualified Text.Parsec.Expr as Parsec
import Text.Parsec hiding ((<|>))
import Data.Text (pack, Text)

import Parser.Monad
import Compiler.Monad
import TAC.Program

import Control.Monad (join)
import Control.Monad.Debug

data Expr = Expr
    { exprGraph    :: Graph Insn O O
    , exprResult   :: TacExp
    , exprResultTy :: Type
    }
type ParsedExpr = Parser Expr

type Operator = Parsec.Operator Text ParserState Compiler ParsedExpr

parseExpr :: Parser Expr
parseExpr = join $ P $ buildExpressionParser opTableAboveTernary under
  where under = pure <$> unP parseExprBelowTernary

parseExprBelowTernary :: Parser Expr
parseExprBelowTernary = join $ P $ buildExpressionParser opTableBelowTernary term

-- TODO!!! Fix type!!!
term :: ParsecT Text ParserState Compiler ParsedExpr
term = do
    rv <- unP parseRValue
    pure $ return $ Expr emptyGraph (ValExp rv) $ IntTy Unsigned

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
