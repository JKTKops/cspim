module Compiler.Flags where

import Data.Data
import qualified Data.Set as S

data Flag
     = FDeferOutOfScopeErrors
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Flags = Flags { getFlags :: S.Set Flag }
  deriving Show

noFlags :: Flags
noFlags = Flags S.empty

isFlagSet :: Flag -> Flags -> Bool
isFlagSet flag (Flags flags) = S.member flag flags

setFlag :: Flag -> Flags -> Flags
setFlag f (Flags flags) = Flags $ S.insert f flags

unsetFlag :: Flag -> Flags -> Flags
unsetFlag f (Flags flags) = Flags $ S.delete f flags
