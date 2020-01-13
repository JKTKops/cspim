module MIPS.Peephole
    ( runMipsPeepholeFwd, runMipsPeepholeBwd
    , instOpt, instTrans
    , module Export
    ) where

import Compiler.Peephole
import MIPS.Language

import Control.Arrow (second)
import Control.Lens
import Data.Maybe (fromJust)

import qualified Compiler.Peephole as Export hiding
       ( analyzeAndRewriteFwd, analyzeAndRewriteBwd
       , analyzeThenRewriteFwd, analyzeThenRewriteBwd
       , rewriteFwd, rewriteBwd, analyzeFwd, analyzeBwd
       )

runMipsPeepholeFwd :: Monad m => FwdPass MipsDeclaration m f -> [MipsLine] -> f
                   -> m (f, [MipsLine])
runMipsPeepholeFwd pass lines fact =
    let opt = analyzeAndRewriteFwd $ modifyFwdPass
                declToLineTrans (adjustFwdRewrite declToLineFun)
                pass
        (comms, optLines) = breakProgForOpt lines
        result = threadOpt opt optLines fact
    in do (finalFact, finalLines) <- result
          return (finalFact, repairProg (comms, finalLines))
  where
    declToLineTrans = adjustFwdTransfer (\f (ML (Just decl) _) -> f decl)

    declToLineFun k lines f = do
        mrepl <- k decls f
        return $ case mrepl of
            Just repl -> Just $ map (\d -> ML (Just d) Nothing) repl
            Nothing   -> Nothing
      where decls = map (\(ML (Just d) _) -> d) lines

runMipsPeepholeBwd :: Monad m => BwdPass MipsDeclaration m f -> [MipsLine] -> f
                   -> m (f, [MipsLine])
runMipsPeepholeBwd pass lines fact =
    let opt = analyzeAndRewriteBwd $ modifyBwdPass
                declToLineTrans (adjustBwdRewrite declToLineFun)
                pass
        (comms, optLines) = breakProgForOpt lines
        result = threadOpt opt (reverse optLines) fact
    in do (finalFact, finalLines) <- result
          return (finalFact, repairProg (comms, reverse finalLines))
  where
    declToLineTrans = adjustBwdTransfer (\f (ML (Just decl) _) -> f decl)

    declToLineFun k lines f = do
        mrepl <- k decls f
        return $ case mrepl of
            Just repl -> Just $ map (\d -> ML (Just d) Nothing) repl
            Nothing   -> Nothing
      where decls = map (\(ML (Just d) _) -> d) lines

threadOpt :: Monad m => ([inst] -> f -> m (f, [inst])) -> [[inst]] -> f -> m (f, [[inst]])
threadOpt opt = loop
  where
    loop (decls:rest) fact = do
        (fact', optimized) <- opt decls fact
        second (optimized:) <$> loop rest fact'
    loop [] fact = return (fact, [])

breakProgForOpt :: [MipsLine] -> ([[String]], [[MipsLine]])
breakProgForOpt lines = comment (filter (not . emptyLine) lines) ([], [])
  where
    comment :: [MipsLine] -> ([[String]], [[MipsLine]])
            -> ([[String]], [[MipsLine]])
    comment lines (comms, clines) =
        let (onlyComms, rest) = span onlyComm lines
            next_st = (map getComm onlyComms : comms, clines)
        in  case rest of
            [] -> next_st
            _  -> declaration rest next_st

    declaration lines (comms, clines) =
        let (hasDecls, rest) = span hasDecl lines
            next_st = (comms, hasDecls : clines)
        in  case rest of
            [] -> next_st
            _  -> comment rest next_st

    onlyComm (ML Nothing (Just _)) = True
    onlyComm _ = False

    getComm (ML Nothing (Just c)) = c

    hasDecl (ML (Just _) _) = True
    hasDecl _ = False

    emptyLine (ML Nothing Nothing) = True
    emptyLine _ = False

repairProg :: ([[String]], [[MipsLine]]) -> [MipsLine]
repairProg = concat . repairProgC
  where
    repairProgC (comm:comms, lines) = map (ML Nothing . Just) comm : repairProgD (comms, lines)
    repairProgC ([], []) = []
    repairProgC ([], lines) = repairProgD ([], lines)

    repairProgD (comms, lines:rest) = lines : repairProgC (comms, rest)
    repairProgD ([], []) = []
    repairProgD (comms, []) = repairProgC (comms, [])

instOpt :: Monad m => ([MipsInstruction] -> f -> m (Maybe [MipsDeclaration]))
        -> OptFun MipsDeclaration m f
instOpt f mds fact = if any (hasn't _Inst) mds
                     then return Nothing
                     else f (map (fromJust . preview _Inst) mds) fact

instTrans :: (MipsInstruction -> f -> f) -> MipsDeclaration -> f -> f
instTrans trans (MInst inst) = trans inst
instTrans trans _            = id
