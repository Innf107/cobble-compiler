module Language.Cobble.Util where

import Language.Cobble.Prelude
  
mapCompose ::  (f (g a) -> f' (g' b)) -> Compose f g a -> Compose f' g' b
mapCompose f = Compose . f . getCompose

todo :: a -> a
todo x = x
{-# WARNING todo "TODO" #-}

unsafeTail :: NonEmpty a -> NonEmpty a
unsafeTail (_ :| []) = error "unsafeTail: only one element"
unsafeTail (_ :| (x:xs)) = x :| xs 
