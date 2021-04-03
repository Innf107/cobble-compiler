module Language.Cobble.Util where

import Language.Cobble.Prelude
  
mapCompose ::  (f (g a) -> f' (g' b)) -> Compose f g a -> Compose f' g' b
mapCompose f = Compose . f . getCompose
  
