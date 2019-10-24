{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module VM.Parser where

import Text.Read
import Data.Functor

import Data.Array
import Data.Char (isSpace)
import Data.List (mapAccumL)

import VM.Language

data ParseError = ParseErrorAtLine Int String

instance Show ParseError where
    show (ParseErrorAtLine line code) = "Parse error: " ++ show line ++ ": " ++ code

parseVM :: String -> Either [ParseError] VmProgram
parseVM s = let instStrings = filter (\(_, l) -> not $ all isSpace l) $ zip [1..] $ lines s
                accum errs (line, instString) = case reads instString of
                    [(inst, _)] -> (errs, inst)
                    _ -> (\tail -> errs [ParseErrorAtLine line instString] ++ tail, VmTerminate)

                (diff, insts) = mapAccumL accum id instStrings
                errs = diff []

            in case errs of
                   [] -> Right $ listArray (0, fromIntegral $ length insts - 1) insts
                   _  -> Left errs

instance Read InstType where
    readPrec =     (lexIdent "byte"     $> InstByte)
               <++ (lexIdent "short"    $> InstShort)
               <++ (lexIdent "unsigned" $> InstUnsigned)
               <++ (lexIdent "signed"   $> InstSigned)
               <++ (lexIdent "float"    $> InstFloat)
               <++ (lexIdent "double"   $> InstDouble)

    readListPrec = readListPrecDefault

instance Read VirtualMemorySegment where
    readPrec =     (lexIdent "static"  $> VmsStatic)
               <++ (lexIdent "arg"     $> VmsArg)
               <++ (lexIdent "local"   $> VmsLocal)
               <++ (lexIdent "pointer" $> VmsPointer)
               <++ (lexIdent "struct"  $> VmsStruct)
               <++ (lexIdent "array"   $> VmsArray)

    readListPrec = readListPrecDefault

instance Read VmAddress where
    readPrec = VmAddress <$> readPrec <*> readPrec
    readListPrec = readListPrecDefault

instance Read VmInstruction where
    readPrec =     (do lexIdent "push"
                       i_type <- readPrec
                       arg <- (Left <$> readPrec) <++ (Right <$> readPrec)
                       return $ VmPush i_type arg)

               <++ (lexIdent "pop" >> VmPop <$> readPrec <*> readPrec)
               <++ (do Ident label <- lexP
                       Symbol ":"  <- lexP
                       return $ VmLabel label)
               <++ (do lexIdent "goto"
                       Ident label <- lexP
                       return $ VmGoto label)
               <++ (do lexIdent "ifgoto"
                       Ident label <- lexP
                       return $ VmIfGoto label)
               <++ (do lexIdent "function"
                       Ident label <- lexP
                       args <- readPrec
                       lcls <- readPrec
                       return $ VmFunc label args lcls)
               <++ (do lexIdent "call"
                       Ident label <- lexP
                       args <- readPrec
                       return $ VmCall label args)
               <++ (lexIdent "return" $> VmReturn)

               <++ (lexIdent "and"  $> VmAnd)
               <++ (lexIdent "or"   $> VmOr)
               <++ (lexIdent "nor"  $> VmNor)
               <++ (lexIdent "xor"  $> VmXor)
               <++ (lexIdent "not"  $> VmNot)
               <++ (lexIdent "comp" $> VmComp)

               <++ (lexOp "eq" >>= return <$> \case
                           None -> VmEq
                           DotS -> VmEqS
                           DotD -> VmEqD)
               <++ (lexOp "ne" >>= return <$> \case
                           None -> VmNe
                           DotS -> VmNeS
                           DotD -> VmNeD)
               <++ (lexOp "ge" >>= return <$> \case
                           None -> VmGe
                           DotS -> VmGeS
                           DotD -> VmGeD)
               <++ (lexOp "gt" >>= return <$> \case
                           None -> VmGt
                           DotS -> VmGtS
                           DotD -> VmGtD)
               <++ (lexOp "le" >>= return <$> \case
                           None -> VmLe
                           DotS -> VmLeS
                           DotD -> VmLeD)
               <++ (lexOp "lt" >>= return <$> \case
                           None -> VmLt
                           DotS -> VmLtS
                           DotD -> VmLtD)

               <++ (lexIdent "geu" $> VmGeu)
               <++ (lexIdent "gtu" $> VmGtu)
               <++ (lexIdent "leu" $> VmLeu)
               <++ (lexIdent "ltu" $> VmLtu)

               <++ (lexOp "add" >>= return <$> \case
                           None -> VmAdd
                           DotS -> VmAddS
                           DotD -> VmAddD)
               <++ (lexOp "sub" >>= return <$> \case
                           None -> VmSub
                           DotS -> VmSubS
                           DotD -> VmSubD)
               <++ (lexOp "mul" >>= return <$> \case
                           None -> VmMul
                           DotS -> VmMulS
                           DotD -> VmMulD)
               <++ (lexOp "div" >>= return <$> \case
                           None -> VmDiv
                           DotS -> VmDivS
                           DotD -> VmDivD)

               <++ (lexIdent "sll"  $> VmSll)

               <++ (lexIdent "addu" $> VmAddU)
               <++ (lexIdent "subu" $> VmSubU)
               <++ (lexIdent "mulu" $> VmMulU)
               <++ (lexIdent "divu" $> VmDivU)

               <++ (lexIdent "terminate" $> VmTerminate)

    readListPrec = readListPrecDefault

lexIdent :: String -> ReadPrec ()
lexIdent s = do
    Ident ((== s) -> True) <- lexP
    return ()

data Completer = None | DotS | DotD

lexOp :: String -> ReadPrec Completer
lexOp s = do
    Ident ((== s) -> True) <- lexP
    parseCompleter <++ return None
  where parseCompleter = do
            Symbol "." <- lexP
            (lexIdent "s" $> DotS) <++ (lexIdent "d" $> DotD)
