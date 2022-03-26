module Language.Cobble.Util.Bitraversable where

import Language.Cobble.Prelude
import qualified Data.Map as M

firstM :: (Bitraversable p, Applicative f) => (a -> f c) -> p a b -> f (p c b)
firstM f = bitraverse f pure 
  
secondM :: (Bitraversable p, Applicative f) => (b -> f c) -> p a b -> f (p a c)
secondM = bitraverse pure 

bimapMap :: (Ord k2) => (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
bimapMap kf vf = M.mapKeys kf . M.map vf

mapBothMap :: (Ord k2)=> (k1 -> v1 -> (k2, v2)) -> Map k1 v1 -> Map k2 v2
mapBothMap f = M.fromAscList . fmap (uncurry f) . M.toAscList
