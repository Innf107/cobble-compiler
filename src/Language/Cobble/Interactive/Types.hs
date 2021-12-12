module Language.Cobble.Interactive.Types where

import Language.Cobble.Prelude
import Language.Cobble (CompilationError)

data InteractiveOutput = InteractiveError CompilationError
                       | LuaOutput Text
                       deriving (Show, Eq)

