{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.Type where

import Pretty
import Parser.Monad
import Compiler.Monad (panic)
import TAC.Program

import Data.Text (unpack)
import Data.Functor (($>))
import Control.Monad
import Control.Monad.Extra
import Control.Lens
import Control.Lens.TH

data PrimTypeSpecifier
data SignageSpecifier

data TypeSpecifier
     = TSInt
     | TSShort
     | TSChar

     | TSSigned
     | TSUnsigned
    deriving (Eq, Show)

pattern PrimTypeSpec <- (isPrimTypeSpec -> True)
pattern SignTypeSpec <- (isSignTypeSpec -> True)

isPrimTypeSpec = (`elem` [TSInt, TSShort, TSChar])
isSignTypeSpec = (`elem` [TSSigned, TSUnsigned])

data ResolvingTypeSpecifiers = ResolvingTS
     { _resolvingType    :: Maybe (ResolvingType, TypeSpecifier)
     , _resolvingSignage :: Maybe (Signage, TypeSpecifier)
     }

data ResolvingType
     = RNeedsSignage (Signage -> Type)
     | RType         Type

resolvingTypeOf :: TypeSpecifier -> Parser ResolvingType
resolvingTypeOf TSInt   = pure $ RNeedsSignage IntTy
resolvingTypeOf TSShort = pure $ RNeedsSignage ShortTy
resolvingTypeOf TSChar  = pure $ RNeedsSignage CharTy
resolvingTypeOf ts      = panic $ "Parser.Type.resolvingTypeOf: "
                               <> pretty ts <> " is not a primitive type name"

signageOfTypeSpec :: TypeSpecifier -> Parser Signage
signageOfTypeSpec TSUnsigned = pure Unsigned
signageOfTypeSpec TSSigned   = pure Signed
signageOfTypeSpec ts         = panic $ "Parser.Type.signageOfTypeSpec: "
                                    <> pretty ts <> " is not a type signage specifier"

resolveType :: ResolvingType -> Signage -> Type
resolveType (RNeedsSignage f) = f
resolveType (RType ty)        = const ty

makeLenses ''ResolvingTypeSpecifiers

parseType :: Parser Type
parseType = finalize <$> start (ResolvingTS Nothing Nothing) <?> "type"
  where start :: ResolvingTypeSpecifiers -> Parser ResolvingTypeSpecifiers
        start rts = do
            spec <- typeSpecifier
            resolveTypeSpecifier rts spec >>= go

        go :: ResolvingTypeSpecifiers -> Parser ResolvingTypeSpecifiers
        go rts = do
            mspecifier <- optionMaybe typeSpecifier
            case mspecifier of
                Nothing -> pure rts
                Just spec -> resolveTypeSpecifier rts spec >>= go

        finalize :: ResolvingTypeSpecifiers -> Type
        finalize (ResolvingTS mrty mrs) =
            let rty = maybe (RNeedsSignage IntTy) fst mrty
                rs  = maybe Signed fst mrs
            in resolveType rty rs

typeSpecifier :: Parser TypeSpecifier
typeSpecifier = (reserved "int"      $> TSInt)
            <|> (reserved "short"    $> TSShort)
            <|> (reserved "char"     $> TSChar)
            <|> (reserved "signed"   $> TSSigned)
            <|> (reserved "unsigned" $> TSUnsigned)

resolveTypeSpecifier :: ResolvingTypeSpecifiers
                     -> TypeSpecifier
                     -> Parser ResolvingTypeSpecifiers
resolveTypeSpecifier r@(ResolvingTS mrty _) p@PrimTypeSpec = case mrty of
    Just (_, ts) -> typeError $ incompatErrorFor ts p
    Nothing      -> do
        rty <- resolvingTypeOf p
        return $ r & resolvingType ?~ (rty, p)
resolveTypeSpecifier r@(ResolvingTS _ mrs) s@SignTypeSpec = case mrs of
    Just (_, ts) -> typeError $ incompatErrorFor ts s
    Nothing -> do
        rs <- signageOfTypeSpec s
        return $ r & resolvingSignage ?~ (rs, s)

data ParseTypeError
     = DuplicateSpecifier TypeSpecifier
     | IncompatibleSpecifiers TypeSpecifier TypeSpecifier

incompatErrorFor :: TypeSpecifier -> TypeSpecifier -> ParseTypeError
incompatErrorFor ts1 ts2
  | ts1 == ts2 = DuplicateSpecifier ts1
  | otherwise  = IncompatibleSpecifiers ts1 ts2

instance Pretty TypeSpecifier where
    pretty TSInt      = "int"
    pretty TSShort    = "short"
    pretty TSChar     = "char"
    pretty TSSigned   = "signed"
    pretty TSUnsigned = "unsigned"

instance Pretty ParseTypeError where
    pretty (DuplicateSpecifier ts) = "Duplicate type specifier: " <> pretty ts
    pretty (IncompatibleSpecifiers ts1 ts2) =
        "Incompatible type specifiers: " <> pretty ts1 <> ", " <> pretty ts2

typeError :: ParseTypeError -> Parser a
typeError e = fail $ unpack $ pretty e
