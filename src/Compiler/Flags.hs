{-# LANGUAGE TypeFamilies #-}
module Compiler.Flags where

import Data.Data
import qualified Data.Set as S
import Data.Char
import Control.Lens

data Flag
     = Verbose
     | E -- stop after preprocessing

     | FDeferOutOfScopeErrors

     | DumpTac
     | DumpSimpl
     | DumpToFile
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Flags = Flags { getFlags :: S.Set Flag }
  deriving Show

type instance Index Flags = Flag
instance Contains Flags where
    contains k f (Flags s) = Flags <$> contains k f s

noFlags :: Flags
noFlags = Flags S.empty

isFlagSet :: Flag -> Flags -> Bool
isFlagSet flag (Flags flags) = S.member flag flags

(?) :: Flags -> Flag -> Bool
(?) = flip isFlagSet

setFlag :: Flag -> Flags -> Flags
setFlag f (Flags flags) = Flags $ S.insert f flags

unsetFlag :: Flag -> Flags -> Flags
unsetFlag f (Flags flags) = Flags $ S.delete f flags

isDumpFlag :: Flag -> Bool
isDumpFlag = (`elem` [DumpTac, DumpSimpl])

flagSkewerCase :: Flag -> String
flagSkewerCase flag = go0 $ show flag
  where go0 (c:cs) = toLower c : camelToSkewer cs
        camelToSkewer []     = []
        camelToSkewer (c:cs) | isUpper c = '-' : toLower c : camelToSkewer cs
                             | otherwise = c : camelToSkewer cs

dumpExtension :: Flag -> String
dumpExtension DumpTac = "dump-tac"
dumpExtension DumpSimpl = "dump-simpl"
