module TAC.Program
    ( Program(..)
    , module TAC.Language
    ) where

import TAC.Language
import Compiler.SymbolTable

data Program = Prog
     { functions   :: [Function]
     , constants   :: [Constant] -- We'll put integer constants if we find them
                                 -- but constant prop will remove them eventually
     , globalVars  :: [Name]
     , symbolTable :: SymbolTable
     }
