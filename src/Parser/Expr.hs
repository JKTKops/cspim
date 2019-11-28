module Parser.Expr where

import Text.Parsec.Expr hiding (Operator)
import qualified Text.Parsec.Expr as Parsec
import Text.Parsec hiding ((<|>))
import Data.Text (unpack, Text)

import Parser.Monad
import Compiler.Monad (Compiler)
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
parseExpr = join $ P $ buildExpressionParser opTableBelowTernary term

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
        p14 = []

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

binary :: String -> Binop -> Assoc -> Operator
binary name op = Infix parse
  where
    parse :: ParsecT Text ParserState Compiler (ParsedExpr -> ParsedExpr -> ParsedExpr)
    parse = unP $ reservedOp name >> connect

    connect :: Parser (ParsedExpr -> ParsedExpr -> ParsedExpr)
    connect = pure $ \e1 e2 -> do
        Expr lg lr lty <- e1
        Expr rg rr rty <- e2
        let outTy = commonRealType lty rty
        (gatherLeft, leftValue) <- case lr of
            ValExp rv -> return (emptyGraph, rv)
            _ -> do ltemp <- mkFreshLocalTempUniq lty
                    return (mkMiddle $ LVar ltemp := lr, RVar (Left ltemp))
        (gatherRight, rightValue) <- case rr of
            ValExp rv -> return (emptyGraph, rv)
            _ -> do rtemp <- mkFreshLocalTempUniq rty
                    return (mkMiddle $ LVar rtemp := rr, RVar (Left rtemp))
        return $ Expr (lg <*|*> gatherLeft <*|*> rg <*|*> gatherRight)
                      (Binop leftValue op rightValue)
                      outTy

commonRealType :: Type -> Type -> Type
commonRealType ty1 ty2
    | isIntegralTy ty1
    , isIntegralTy ty2 = makeSignageAgree (promote ty1) (promote ty2)
    | otherwise = error "Parser.Expr.commonRealType: Non-integral types"

  where promote :: Type -> Type
        promote (IntTy Unsigned) = IntTy Unsigned
        promote _                = IntTy Signed

        makeSignageAgree :: Type -> Type -> Type
        makeSignageAgree ty1 ty2
          | IntTy Unsigned <- ty1 = IntTy Unsigned
          | IntTy Unsigned <- ty2 = IntTy Unsigned
          | otherwise             = IntTy Signed
