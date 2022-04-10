{-#LANGUAGE TemplateHaskell#-}
module Cobble.Types.Lens where

import Cobble.Prelude
import Cobble.Types.AST
import Cobble.Types.QualifiedName

makeLensesWith abbreviatedFields ''ModSig

passCoerced :: (CoercePass a b, CoercePass b a) => Iso' a b
passCoerced = iso coercePass coercePass 
