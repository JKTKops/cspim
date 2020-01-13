module MIPS.Peephole.Pass where

import MIPS.Peephole.CleanJumps
import MIPS.Peephole.Liveness
import MIPS.Language

import Control.Monad.Identity

runMipsPeepholePass :: Program -> Program
runMipsPeepholePass prog = runIdentity $ pass prog
  where pass = liveness >=> cleanJumps
