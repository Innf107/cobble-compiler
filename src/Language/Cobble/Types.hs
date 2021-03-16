module Language.Cobble.Types  
    (
      module Export
    , NameSpace
    )
    where

import Language.Cobble.Types.AST as Export
import Language.Cobble.Types.AST.QualifyNames as Export
import Language.Cobble.Types.AST.Typecheck as Export
import Language.Cobble.Types.AST.Codegen as Export
import Language.Cobble.Shared as Export

import Language.Cobble.Prelude

type NameSpace = Text