{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.Lens where

import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.QualifiedName

makeLensesWith abbreviatedFields ''ModSig

makeLenses ''StructDef

fieldType :: UnqualifiedName -> Fold StructDef Type
fieldType n = structFields . ifolded . ifiltered (const $ (==n) . fst) . _2

passCoerced :: (CoercePass a b, CoercePass b a) => Iso' a b
passCoerced = iso coercePass coercePass 
