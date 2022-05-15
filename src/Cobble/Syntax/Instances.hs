{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Cobble.Syntax.Instances where

import Cobble.Prelude

import Cobble.Syntax.AST
import Cobble.Syntax.TH

deriveInstanceReqs

