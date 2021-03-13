{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies#-}
module Language.Cobble.Types.AST.ParsePreprocess where

import Language.Cobble.Prelude
import Language.Cobble.Types.AST
  
type instance Name 'ParsePreprocess = Text

