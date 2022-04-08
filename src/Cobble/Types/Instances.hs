{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Cobble.Types.Instances where

import Cobble.Prelude

import Cobble.Types.AST
import Cobble.Types.TH

deriveInstanceReqs

