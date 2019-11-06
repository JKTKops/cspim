{-# LANGUAGE TemplateHaskell #-}
module TAC.Program
    ( module TAC.Program
    , module TAC.Language
    ) where

import TAC.Language hiding ((<*>))
import qualified TAC.Language as Hoopl ((<*>))
import Compiler.SymbolTable
import Control.Lens.TH

(<*|*>) :: NonLocal n => Graph n e O -> Graph n O x -> Graph n e x
(<*|*>) = (Hoopl.<*>)

data Program = Prog
     { _functions   :: [Function]
     , _constants   :: [Constant] -- We'll put integer constants if we find them
                                  -- but constant prop will remove them eventually
                                  -- This info isn't complete and requires a label
     , _globalVars  :: [Name]
     , _symbolTable :: SymbolTable
     }

makeLenses ''Program
