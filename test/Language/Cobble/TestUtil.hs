{-#OPTIONS_GHC -Wno-orphans#-}
module Language.Cobble.TestUtil where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

instance IsString QualifiedName where
    fromString = internalQName . toText

pattern QName :: Text -> QualifiedName
pattern QName name <- UnsafeQualifiedName name _ _

