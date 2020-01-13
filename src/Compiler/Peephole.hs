-- | Peephole optimization of assembly-like code.
-- Intended to be similar to Hoopl in API.
-- Hoopl enables interleaved optimizations by using a neat transducer trick
-- where each Rewrite returns both the new graph AND the next rewrite to try.
-- aka, FwdRewrite n m f ~ n -> f -> m (Maybe (graph, FwdRewrite n m f))
-- I'm not implementing that for now.
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Compiler.Peephole
    ( -- * Optimization functions
      OptFun, PeepSize, emptyOptFun, contraMapOpt
    -- * Rewrites and transfers
    , FwdRewrite, BwdRewrite, fwdRewrite, bwdRewrite
    , FwdTransfer, BwdTransfer, fwdTransfer, bwdTransfer
    , noFwdRewrite, noBwdRewrite, noFwdTransfer, noBwdTransfer
    , adjustFwdTransfer, adjustBwdTransfer
    , adjustFwdRewrite, adjustBwdRewrite
    , combineFwdRewrite, combineBwdRewrite
    -- * Making Passes
    , FwdPass, BwdPass, fwdPass, bwdPass
    , interleaveFwdPass, interleaveBwdPass
    , pairFwdPass, pairBwdPass
    , modifyFwdPass, modifyBwdPass
    -- * Consuming Passes
    , analyzeAndRewriteFwd, analyzeAndRewriteBwd
    , analyzeThenRewriteFwd, analyzeThenRewriteBwd
    , analyzeFwd, analyzeBwd, rewriteFwd, rewriteBwd
    ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Foldable (asum)
import Control.Arrow (second)
import Control.Lens

import qualified Data.Zipper.List as ZL
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Program inst = ZL.Zipper inst

newtype OptimizeT inst m a =
    OptimizeT { unOptimizeT :: StateT (Program inst) (WriterT Change m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (OptimizeT inst) where
    lift = OptimizeT . lift . lift
data Change = Changed | NoChange deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Semigroup Change where NoChange <> NoChange = NoChange; _ <> _ = Changed
instance Monoid Change where
    mappend = (<>)
    mempty = NoChange

runOptimizeT :: Monad m => OptimizeT inst m a -> [inst] -> m (a, [inst])
runOptimizeT (OptimizeT m) p = m
                             & flip runStateT (ZL.fromList p)
                             & fmap fst . runWriterT
                             & fmap (second ZL.toList)

getProg :: Monad m => OptimizeT inst m (Program inst)
getProg = OptimizeT get

putUnchanged, putChanged :: Monad m => Program inst -> OptimizeT inst m ()
putUnchanged = OptimizeT . put
putChanged p = sayChanged *> OptimizeT (put p)

-- | Move the program held by the optimizer. Uses a rank 2 type to help protect against
--   accidentally modifying the /program/ rather than just moving the zipper.
moveProg :: Monad m => (forall inst. Program inst -> Program inst) -> OptimizeT inst m ()
moveProg = OptimizeT . modify

sendProg :: Monad m => (Program inst -> m a) -> OptimizeT inst m a
sendProg f = OptimizeT $ get >>= lift . lift . f

withProg :: Monad m => (Program inst -> OptimizeT inst m ()) -> OptimizeT inst m ()
withProg f = OptimizeT $ join $ gets (unOptimizeT . f)

sayChanged :: Monad m => OptimizeT inst m ()
sayChanged = OptimizeT $ tell Changed

listenChanged :: Monad m => OptimizeT inst m a -> OptimizeT inst m (a, Change)
listenChanged t = OptimizeT $ listen (unOptimizeT t)

-- | Take an OptimizeT action and iterate it until it returns NoChange.
optFix :: Monad m => OptimizeT inst m a -> OptimizeT inst m [a]
optFix opt = loop where
  loop = do
    (a0, change) <- listenChanged opt
    case change of
        NoChange -> return [a0]
        Changed  -> liftA2 (:) (pure a0) loop

optFix_ :: Monad m => OptimizeT inst m a -> OptimizeT inst m ()
optFix_ opt = loop where
  loop = do
    (_, change) <- listenChanged opt
    case change of
        NoChange -> return ()
        Changed  -> loop

type OptFun inst m f = [inst] -> f -> m (Maybe [inst])
type PeepSize = Int
data Optimization inst m f =
    FwdOpt (FwdRewrite inst m f)
  | BwdOpt (BwdRewrite inst m f)

emptyOptFun :: Applicative m => OptFun inst m f
emptyOptFun _ _ = pure Nothing

contraMapOpt :: (f' -> f) -> OptFun inst m f -> OptFun inst m f'
contraMapOpt fun opt insts f' = opt insts (fun f')

-- TODO: for MIPS, write functions (FwdRewrite MipsDeclaration m f -> FwdRewrite MipsLine m f)
-- &c.
newtype FwdRewrite inst m f = FwdRewrite (PeepSize, OptFun inst m f)
newtype BwdRewrite inst m f = BwdRewrite (PeepSize, OptFun inst m f)
newtype FwdTransfer inst f = FwdTransfer { runFwdTransfer :: inst -> f -> f }
newtype BwdTransfer inst f = BwdTransfer { runBwdTransfer :: inst -> f -> f }

fwdRewrite :: PeepSize -> OptFun inst m f -> FwdRewrite inst m f
fwdRewrite s r = FwdRewrite (s, r)
bwdRewrite :: PeepSize -> OptFun inst m f -> BwdRewrite inst m f
bwdRewrite s r = BwdRewrite (s, r)

fwdTransfer :: (inst -> f -> f) -> FwdTransfer inst f
bwdTransfer :: (inst -> f -> f) -> BwdTransfer inst f
fwdTransfer = FwdTransfer
bwdTransfer = BwdTransfer

adjustFwdTransfer :: ((inst -> f -> f) -> (inst' -> f' -> f'))
                  -> FwdTransfer inst f -> FwdTransfer inst' f'
adjustFwdTransfer f (FwdTransfer t) = fwdTransfer (f t)

adjustBwdTransfer :: ((inst -> f -> f) -> inst' -> f' -> f')
                  -> BwdTransfer inst f -> BwdTransfer inst' f'
adjustBwdTransfer f (BwdTransfer t) = bwdTransfer (f t)

noFwdRewrite :: Applicative m => FwdRewrite inst m f
noBwdRewrite :: Applicative m => BwdRewrite inst m f
noFwdRewrite = FwdRewrite (0, emptyOptFun)
noBwdRewrite = BwdRewrite (0, emptyOptFun)
noFwdTransfer :: FwdTransfer inst f
noBwdTransfer :: BwdTransfer inst f
noFwdTransfer = FwdTransfer $ const id
noBwdTransfer = BwdTransfer $ const id

adjustFwdRewrite :: (OptFun inst m f -> OptFun inst' m' f') -> FwdRewrite inst m f
                 -> FwdRewrite inst' m' f'
adjustFwdRewrite g (FwdRewrite (size, o)) = FwdRewrite (size, g o)
adjustBwdRewrite g (BwdRewrite (size, o)) = BwdRewrite (size, g o)

class EndoArrow a where
    (^>>) :: (b' -> b) -> a b -> a b'
instance EndoArrow (FwdRewrite inst m) where
    (^>>) f = adjustFwdRewrite (contraMapOpt f)
instance EndoArrow (BwdRewrite inst m) where
    (^>>) f = adjustBwdRewrite (contraMapOpt f)

combineFwdRewrite :: Monad m => FwdRewrite inst m f -> FwdRewrite inst m f
                  -> FwdRewrite inst m f
combineFwdRewrite (FwdRewrite (s1, o1)) (FwdRewrite (s2, o2)) = FwdRewrite (max s1 s2, o3)
  where o3 insts f = asum <$> sequence
            [o1 (take s1 insts) f, o2 (take s2 insts) f]

combineBwdRewrite :: Monad m => BwdRewrite inst m f -> BwdRewrite inst m f
                  -> BwdRewrite inst m f
combineBwdRewrite (BwdRewrite (s1, r1)) (BwdRewrite (s2, r2)) = BwdRewrite (max s1 s2, r3)
  where r3 insts f = asum <$> sequence
            [ r1 (reverse . take s1 $ reverse insts) f
            , r2 (reverse . take s2 $ reverse insts) f]

instance Monad m => Semigroup (FwdRewrite inst m f) where
    (<>) = combineFwdRewrite
instance Monad m => Monoid (FwdRewrite inst m f) where
    mempty = noFwdRewrite

instance Monad m => Semigroup (BwdRewrite inst m f) where
    (<>) = combineBwdRewrite
instance Monad m => Monoid (BwdRewrite inst m f) where
    mempty = noBwdRewrite

data FwdPass inst m f = FwdPass (FwdTransfer inst f) (FwdRewrite inst m f)
data BwdPass inst m f = BwdPass (BwdTransfer inst f) (BwdRewrite inst m f)

fwdPass :: FwdTransfer inst f -> FwdRewrite inst m f -> FwdPass inst m f
bwdPass :: BwdTransfer inst f -> BwdRewrite inst m f -> BwdPass inst m f
fwdPass = FwdPass
bwdPass = BwdPass

modifyFwdPass :: (FwdTransfer inst f -> FwdTransfer inst' f')
              -> (FwdRewrite inst m f -> FwdRewrite inst' m' f')
              -> FwdPass inst m f -> FwdPass inst' m' f'
modifyFwdPass f g (FwdPass t r) = FwdPass (f t) (g r)

modifyBwdPass :: (BwdTransfer inst f -> BwdTransfer inst' f')
              -> (BwdRewrite inst m f -> BwdRewrite inst' m' f')
              -> BwdPass inst m f -> BwdPass inst' m' f'
modifyBwdPass f g (BwdPass t r) = BwdPass (f t) (g r)

interleaveFwdPass :: Monad m => FwdPass inst m f -> FwdPass inst m f -> FwdPass inst m f
interleaveFwdPass (FwdPass t1 r1) (FwdPass t2 r2) =
    let transfer inst f0 = f2 where
          f1 = runFwdTransfer t1 inst f0
          f2 = runFwdTransfer t2 inst f1
        rewrite = r1 <> r2
    in FwdPass (FwdTransfer transfer) rewrite

interleaveBwdPass :: Monad m => BwdPass inst m f -> BwdPass inst m f -> BwdPass inst m f
interleaveBwdPass (BwdPass t1 r1) (BwdPass t2 r2) =
    let transfer inst f0 = f2 where
          f1 = runBwdTransfer t1 inst f0
          f2 = runBwdTransfer t2 inst f1
        rewrite = r1 <> r2
    in BwdPass (BwdTransfer transfer) rewrite

pairFwdPass :: Monad m => FwdPass inst m f -> FwdPass inst m f' -> FwdPass inst m (f, f')
pairFwdPass (FwdPass tf r1) (FwdPass tf' r2) =
    let transfer inst (f0, f0') = (f1, f1') where
          f1  = runFwdTransfer tf  inst f0
          f1' = runFwdTransfer tf' inst f0'
        rewrite = (fst ^>> r1) <> (snd ^>> r2)
    in FwdPass (FwdTransfer transfer) rewrite

pairBwdPass :: Monad m => BwdPass inst m f -> BwdPass inst m f' -> BwdPass inst m (f, f')
pairBwdPass (BwdPass tf r1) (BwdPass tf' r2) =
    let transfer inst (f0, f0') = (f1, f1') where
          f1  = runBwdTransfer tf  inst f0
          f1' = runBwdTransfer tf' inst f0'
        rewrite = (fst ^>> r1) <> (snd ^>> r2)
    in BwdPass (BwdTransfer transfer) rewrite

applyOpt :: Monad m => Optimization inst m f -> f -> OptimizeT inst m ()
applyOpt optimization fact = withProg $ \z@(ZL.Zip _ focus) -> do
    let insts = order $ take size focus
    lift (opt insts fact) >>= \case
        Nothing   -> putUnchanged z
        Just repl -> putChanged $ replace size repl z
  where (order, size, opt) = case optimization of
            FwdOpt (FwdRewrite (s, o)) -> (id, s, o)
            BwdOpt (BwdRewrite (s, o)) -> (reverse, s, o)

        replace :: Int -> [inst] -> Program inst -> Program inst
        replace rm repl (ZL.Zip cs focus) = ZL.Zip cs (repl ++ drop rm focus)

-- | Use a FwdPass to interleave analysis and rewriting.
analyzeAndRewriteFwd :: forall inst m f. Monad m
                     => FwdPass inst m f -> [inst] -> f
                     -> m (f, [inst])
analyzeAndRewriteFwd (FwdPass ftransfer frewrite) insts init_fact =
    runOptimizeT (moveProg ZL.start *> drive init_fact) insts
  where
    applyOpt' = applyOpt (FwdOpt frewrite)

    drive :: f -> OptimizeT inst m f
    drive fact = do
        optFix_ (applyOpt' fact)
        prog <- getProg
        -- guard against an optimization that deletes the whole program
        -- by not pattern matching against a non-empty focus
        let fact' | Just v <- ZL.current prog = runFwdTransfer ftransfer v fact
                  | otherwise = fact
        if ZL.isEnd prog
        then return fact'
        else moveProg (fromJust . ZL.right) *> drive fact'

-- | Use a BwdPass to interleave analysis and rewriting.
analyzeAndRewriteBwd :: forall inst m f. Monad m
                     => BwdPass inst m f -> [inst] -> f
                     -> m (f, [inst])
analyzeAndRewriteBwd (BwdPass btransfer brewrite) insts init_fact =
    second reverse <$> runOptimizeT (moveProg (ZL.start . ZL.reverse) *> drive init_fact) insts
  where
    applyOpt' = applyOpt (BwdOpt brewrite)

    drive :: f -> OptimizeT inst m f
    drive fact = do
        optFix_ (applyOpt' fact)
        prog <- getProg
        let fact' | Just v <- ZL.current prog = runBwdTransfer btransfer v fact
                  | otherwise = fact
        if ZL.isEnd prog
        then return fact'
        else moveProg (fromJust . ZL.right) *> drive fact'

-- | Use a FwdPass to first analyze the whole program, then rewrite it using the
-- results of the analysis.
analyzeThenRewriteFwd :: Monad m => FwdPass inst m f -> [inst] -> f -> m (f, [inst])
analyzeThenRewriteFwd (FwdPass ftransfer frewrite) insts f =
    analyzeFwd ftransfer insts f >>= analyzeAndRewriteFwd (FwdPass noFwdTransfer frewrite) insts

-- | Use a FwdPass to first analyze the whole program, then rewrite it using the
-- results of the analysis.
analyzeThenRewriteBwd :: Monad m => BwdPass inst m f -> [inst] -> f -> m (f, [inst])
analyzeThenRewriteBwd (BwdPass btransfer brewrite) insts f =
    analyzeBwd btransfer insts f >>= analyzeAndRewriteBwd (BwdPass noBwdTransfer brewrite) insts

analyzeFwd :: Monad m => FwdTransfer inst f -> [inst] -> f -> m f
analyzeFwd t is f = fst <$> analyzeAndRewriteFwd (FwdPass t noFwdRewrite) is f

analyzeBwd :: Monad m => BwdTransfer inst f -> [inst] -> f -> m f
analyzeBwd t is f = fst <$> analyzeAndRewriteBwd (BwdPass t noBwdRewrite) is f

rewriteFwd :: Monad m => FwdRewrite inst m () -> [inst] -> m [inst]
rewriteFwd r is = snd <$> analyzeAndRewriteFwd (FwdPass noFwdTransfer r) is ()

rewriteBwd :: Monad m => BwdRewrite inst m () -> [inst] -> m [inst]
rewriteBwd r is = snd <$> analyzeAndRewriteBwd (BwdPass noBwdTransfer r) is ()
