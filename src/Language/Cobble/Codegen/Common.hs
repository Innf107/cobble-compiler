module Language.Cobble.Codegen.Common where

import Language.Cobble.Prelude
import Language.Cobble.Shared
  
freshVar :: (Members '[State Int] r) => QualifiedName -> Sem r QualifiedName
freshVar v = state \i -> (v +. show i, i + 1)