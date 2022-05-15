{-#LANGUAGE TemplateHaskell#-}
module Cobble.Syntax.Lens where

import Cobble.Prelude
import Cobble.Syntax.AST
import Cobble.Syntax.QualifiedName

makeLensesWith abbreviatedFields ''ModSig

passCoerced :: (CoercePass a b, CoercePass b a) => Iso' a b
passCoerced = iso coercePass coercePass 
