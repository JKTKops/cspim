module MIPS.Peephole.CleanJumps where

import MIPS.Peephole
import MIPS.Language

import qualified Data.Map as M

type JumpFact = M.Map Label Int

analyzeJumpCounts :: Monad m => FwdPass MipsDeclaration m JumpFact
analyzeJumpCounts = fwdPass (fwdTransfer $ instTrans countLabelUses) noFwdRewrite
  where
    countLabelUses inst
      | Just lbl <- readLabel inst = M.insertWith (+) lbl 1
      | otherwise                  = id

deleteFallthroughJumpsAndCountLabels :: Monad m => FwdPass MipsDeclaration m JumpFact
deleteFallthroughJumpsAndCountLabels = fwdPass (fwdTransfer $ instTrans countLabelUses)
                                       $ fwdRewrite 2 rewrite
  where
    countLabelUses inst
      | Just lbl <- readLabel inst = M.insertWith (+) lbl 1
      | otherwise                  = id

    rewrite [MInst branch, MLabel lbl] _
      | MJal _      <- branch = return Nothing
      | MBgezal _ _ <- branch = return Nothing
      | MBltzal _ _ <- branch = return Nothing
      | Just lbl' <- branchTarget branch
      , lbl == lbl' = return $ Just [MLabel lbl]
      | otherwise   = return Nothing
    rewrite _ _ = return Nothing

deleteUnusedLabels :: Monad m => FwdPass MipsDeclaration m JumpFact
deleteUnusedLabels = fwdPass noFwdTransfer $ fwdRewrite 1 rewrite
  where
    rewrite [MLabel lbl] fact
      | Nothing <- fact M.!? lbl = return $ Just []
      | otherwise = return Nothing
    rewrite _ _ = return Nothing

cleanJumps :: Monad m => [MipsLine] -> m [MipsLine]
cleanJumps lines = do
    (fact, lines') <- runMipsPeepholeFwd deleteFallthroughJumpsAndCountLabels lines M.empty
    snd <$> runMipsPeepholeFwd deleteUnusedLabels lines' fact
