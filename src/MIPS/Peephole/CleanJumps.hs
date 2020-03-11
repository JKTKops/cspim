{-
Note:

This module used to delete unused labels, but this is not actually valid!

Other files may use those labels. Labels are free anyway, so we can leave them in the code
safely.
-}
module MIPS.Peephole.CleanJumps where

import MIPS.Peephole
import MIPS.Language
import Compiler.SymbolTable

import qualified Data.Map as M

type JumpFact = M.Map Label Int

initJumpFact :: JumpFact
initJumpFact = M.fromList [("main", 1)]

analyzeJumpCounts :: Monad m => FwdPass MipsDeclaration m JumpFact
analyzeJumpCounts = fwdPass (fwdTransfer $ instTrans countLabelUses) noFwdRewrite
  where
    countLabelUses inst
      | Just lbl <- readLabel inst = M.insertWith (+) lbl 1
      | otherwise                  = id

deleteFallthroughJumps :: Monad m => FwdPass MipsDeclaration m JumpFact
deleteFallthroughJumps = fwdPass noFwdTransfer $ fwdRewrite 2 rewrite
  where
    rewrite [MInst branch, MLabel lbl] _
      | MJal _      <- branch = return Nothing
      | MBgezal _ _ <- branch = return Nothing
      | MBltzal _ _ <- branch = return Nothing
      | Just lbl'   <- branchTarget branch
      , lbl == lbl' = return $ Just [MLabel lbl]
      | otherwise   = return Nothing
    rewrite _ _ = return Nothing

cleanJumps :: Monad m => [MipsLine] -> m [MipsLine]
cleanJumps lines = snd <$> runMipsPeepholeFwd deleteFallthroughJumps lines initJumpFact
