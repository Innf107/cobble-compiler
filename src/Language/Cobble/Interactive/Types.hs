module Language.Cobble.Interactive.Types where

import Language.Cobble.Prelude
import Language.Cobble (CompilationError)

import HsLua qualified as Lua

data InteractiveOutput = InteractiveError CompilationError
                       | Success
                       | LuaError Lua.Status
                       deriving (Show, Eq)

