{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

#if defined (LOADED_INTO_GHCI)
{-# LANGUAGE QuasiQuotes #-}
#endif

module MIPS.Peephole.Liveness where

import MIPS.Peephole
import MIPS.Peephole.PeepholeSplices
import MIPS.Language

import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens
import Control.Lens.Utils

import qualified Data.Set as S
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
#if defined(LOADED_INTO_GHCI)
import MIPS.Parser (mips) -- for ghci
import Pretty
import MIPS.Pretty ()
#endif

type LiveFact = S.Set Reg
data Liveness = Alive | Dead deriving (Eq, Ord, Show, Read, Enum, Bounded)
initLiveFact :: LiveFact
initLiveFact = S.fromList [minBound..maxBound]

updLiveness :: MipsInstruction -> LiveFact -> LiveFact
updLiveness mi fact
  | isBranch mi = initLiveFact
  | otherwise   = fact & fromMaybe id (instDest mi >>= \r -> Just (contains r .~ False))
                       & foreach (instSources mi) (\r -> contains r .~ True)

foreach :: [a] -> (a -> s -> s) -> s -> s
foreach funs f init = foldl (flip f) init funs

declFunToLineFun :: Monad m => OptFun MipsDeclaration m f -> OptFun MipsLine m f
declFunToLineFun declFun lines f =
    if any (hasn't declaration) lines
    then return Nothing
    else declFun (map (fromJust . view declaration) lines) f
         <&> fmap (map (\decl -> ML (Just decl) Nothing))

deleteDeadWrite :: Monad m => BwdRewrite MipsDeclaration m LiveFact
deleteDeadWrite = bwdRewrite 1 (instOpt opt) where
  opt [inst] set = runMaybeT $ do
      Just dest <- return $ instDest inst -- instead of let to get MaybeT's MonadFail
      guard $ S.notMember dest set
      return []
  opt [] _ = pure Nothing

deleteWriteTo0 :: Applicative m => BwdRewrite MipsDeclaration m f
deleteWriteTo0 = bwdRewrite 1 opt where
  opt [d] _ | Just Reg0 <- instDest =<< d ^? _Inst = pure (Just [])
  opt _ _ = pure Nothing

livenessPass :: Monad m => BwdPass MipsDeclaration m LiveFact
livenessPass = bwdPass (bwdTransfer $ instTrans updLiveness) (deleteDeadWrite <> deleteWriteTo0)

analyzeLiveness :: Monad m => BwdPass MipsDeclaration m LiveFact
analyzeLiveness = bwdPass (bwdTransfer $ instTrans updLiveness) noBwdRewrite

liveness :: Monad m => [MipsLine] -> m [MipsLine]
liveness lines = snd <$> runMipsPeepholeBwd livenessPass lines initLiveFact

echoNF :: Show inst => PeepSize -> FwdRewrite inst IO f
echoNF n = fwdRewrite n echo

echoNB :: Show inst => PeepSize -> BwdRewrite inst IO f
echoNB n = bwdRewrite n echo

echo insts _ = Nothing <$ print insts

#if defined(LOADED_INTO_GHCI)
-- for ghci
mls = [mips| add $0, $0, $0
             sub $0, $0, $0
             slt $t0, $t0, 0
             add $0, $t0, 15 |]
#endif
