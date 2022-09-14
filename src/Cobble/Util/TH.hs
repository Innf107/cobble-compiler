module Cobble.Util.TH where

import Relude -- Relude to avoid unnecessary name clashes

import Language.Haskell.TH

includeFile :: FilePath -> ExpQ
includeFile path = do
    content <- readFile path
    litE (StringL content)
