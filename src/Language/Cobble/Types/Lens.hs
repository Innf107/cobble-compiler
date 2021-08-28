{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.Lens where

import Language.Cobble.Prelude
import Language.Cobble.Types.AST
import Language.Cobble.Types.QualifiedName

makeLensesWith abbreviatedFields ''ModSig

makeLenses ''StructDef

fieldType :: UnqualifiedName -> Fold (StructDef p) (Type p)
fieldType n = structFields . ifolded . ifiltered (const $ (==n) . fst) . _2

passCoerced :: (CoercePass t1 t2 p1 p2, CoercePass t2 t1 p2 p1) => Iso' t1 t2
passCoerced = iso coercePass coercePass 
