{-|
This is a currently unused module that contains the starts of a pretty printer for
parsec errors. It's a bit complicated because of the preprocessor.

In order to work effectively, the parse errors will need to be fed the
1) original, unpreprocessed source
2) of the file that contains the parse error.

In order to give them a pretty and CompileError instance, we need a data type to hold
a Parsec error + the appropriate source file. In order to _obtain_ that file...
file contents are compile-time constants. I think I know what I'm doing, so
I propose using `unsafePerformIO.`

Since this module was copied from ProtoHaskell, none of that is implemented yet and
it shouldn't be used.
-}


module Parser.Errors where
--    (pprParseError) where

-- import Text.Parsec.Error
-- import Text.Parsec.Pos

-- import Pretty
-- import Text.PrettyPrint
-- import Parser.Lexer

-- import Data.Char

-- pprParseError :: ParseError -> String -> Doc
-- pprParseError pe "" = text $ show pe -- Use text . show instead of ppr in case
--                                         -- definition of ppr changes to use this function
-- pprParseError pe "" = text $ show pe
-- pprParseError pe _  = text $ show pe
-- pprParseError pe src =
--     let pos  = errorPos pe
--         msgs = errorMessages pe
--         line = sourceLine pos
--         col  = sourceColumn pos
--         info = findInfo line col src
--         infoAmount = case info of
--             Nothing -> NoInfo
--             Just _  -> Source
--         spaces = replicate (length (show line) + 1) ' '
--         template src arrows = spaces ++ "|\n" ++
--                               show line ++ " | " ++ src ++ "\n" ++
--                               spaces ++ "| " ++ arrows
--     in mkErrorMessage infoAmount info pos msgs template

-- data InfoAmount = NoInfo | Source

-- findInfo :: Line -> Column -> String -> Maybe String
-- findInfo line col src = searchString src line col

-- searchString :: String -> Line -> Column -> Maybe String
-- searchString src line col = do
--     let srcLines = lines src
--     if length srcLines < line
--       then Nothing
--       else Just $ srcLines !! (line - 1)

-- showPos :: SourcePos -> String
-- showPos p = show (sourceLine p) ++ ":" ++ show (sourceColumn p)

-- mkErrorMessage :: InfoAmount                   -- How detailed can our source/arrows be
--                -> Maybe String                 -- Components for source/arrows
--                -> SourcePos                    -- Position of error
--                -> [Message]                    -- Parsec error messages
--                -> (String -> String -> String) -- Callback to template
--                                                -- source/arrows into msg
--                -> Doc
-- mkErrorMessage infoAmt info pos msgs template =
--     let prettySource = case infoAmt of
--             NoInfo -> mempty
--             Source -> templateSource
--     in header pos $$ prettySource $$ errMsgBody msgs
--   where
--     getBodyAndArrowWs src =
--         let col = sourceColumn pos
--             (leadingWS, body) = span isSpace src
--             initSrcLoc = mkRealSrcLoc "" (sourceLine pos) 1
--             bodyStartLoc = foldl advanceSrcLoc initSrcLoc leadingWS
--             arrowWs = replicate (col - realSrcLocCol bodyStartLoc) ' '
--         in (body, arrowWs)

--     templateSource =
--         let Just src = info
--             (body, arrowWs) = getBodyAndArrowWs src
--             arrows = "^"
--         in text (template body (arrowWs ++ arrows))

-- header :: SourcePos -> Doc
-- header pos = text $ "Parse error at " ++ showPos pos ++ ":"

-- errMsgBody :: [Message] -> Doc
-- errMsgBody = text . showErrorMessages "or" "unknown parse error"
--                                       "expecting" "unexpected" "end of input"
