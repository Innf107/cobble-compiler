module Cobble.Util where

import Cobble.Prelude
  
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

zipFor :: [a] -> [b] -> (a -> b -> c) -> [c]
zipFor x y f = zipWith f x y

zipForM :: (Applicative f) => [a] -> [b] -> (a -> b -> f c) -> f [c]
zipForM x y f = zipWithM f x y

zipForM_ :: (Applicative f) => [a] -> [b] -> (a -> b -> f c) -> f ()
zipForM_ x y f = zipWithM_ f x y

