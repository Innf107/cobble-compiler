module Language.Cobble.Util.Bitraversable where

import Language.Cobble.Prelude

leftM :: (Bitraversable p, Applicative f) => (a -> f c) -> p a b -> f (p c b)
leftM f = bitraverse f pure 
  
rightM :: (Bitraversable p, Applicative f) => (b -> f c) -> p a b -> f (p a c)
rightM = bitraverse pure 
