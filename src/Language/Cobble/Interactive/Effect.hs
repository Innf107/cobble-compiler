{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Interactive.Effect where

import Language.Cobble.Prelude

data Interactive m a where
    Eval :: Text -> Interactive m Text

makeSem ''Interactive
