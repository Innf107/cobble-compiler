module Language.Cobble.Util where

import Language.Cobble.Prelude
  
mapCompose ::  (f (g a) -> f' (g' b)) -> Compose f g a -> Compose f' g' b
mapCompose f = Compose . f . getCompose

todo :: a -> a
todo x = x
{-# WARNING todo "TODO" #-}

unsafeTail :: HasCallStack => NonEmpty a -> NonEmpty a
unsafeTail (_ :| []) = error "unsafeTail: only one element"
unsafeTail (_ :| (x:xs)) = x :| xs 

(<<|>>) :: (Applicative f, Alternative g) => f (g b) -> f (g b) -> f (g b) 
(<<|>>)= liftA2 (<|>)
