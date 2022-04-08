{-#LANGUAGE TemplateHaskell#-}
module Cobble.Types.Lens where

import Cobble.Prelude
import Cobble.Types.AST
import Cobble.Types.QualifiedName

makeLensesWith abbreviatedFields ''ModSig

makeLenses ''StructDef

fieldType :: UnqualifiedName -> Fold StructDef Type
fieldType n = structFields . ifolded . ifiltered (const $ (==n) . fst) . _2

passCoerced :: (CoercePass a b, CoercePass b a) => Iso' a b
passCoerced = iso coercePass coercePass 
