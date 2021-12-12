{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Interactive.Effect where

import Language.Cobble.Prelude
import Language.Cobble.Interactive.Types

data Interactive m a where
    Eval :: Text -> Interactive m InteractiveOutput
    EvalLua :: Text -> Interactive m InteractiveOutput

makeSem ''Interactive
