module Compiler.Flags where

import qualified Data.Set as S

data Flag

type Flags = S.Set Flag
