{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.Lens where

import Language.Cobble.Prelude
import Language.Cobble.Types.AST

makeLensesWith abbreviatedFields ''ModSig

makeLenses ''StructDef