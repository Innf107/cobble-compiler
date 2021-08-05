{-#LANGUAGE UndecidableInstances#-}
module Language.Cobble.Codegen.Common (freshVar, freshen, Range(..), rminmax) where

import Language.Cobble.Prelude
import Language.Cobble.Shared
import Language.Cobble.Util.TypeUtils
import Language.Haskell.TH (Extension(UndecidableInstances))
import Relude (Semigroup)
  
freshVar :: (Members '[State Int] r) => QualifiedName -> Sem r QualifiedName
freshVar v = state \i -> (v +. show i, i + 1)


class Freshen o f | o -> f, f -> o where
    freshen :: (Members '[State Int] r) => o -> Sem r f

instance Freshen QualifiedName QualifiedName where
    freshen = freshVar

instance (Freshen o f) => Freshen [o] [f] where
    freshen = traverse freshen

instance (Freshen a a', Freshen b b') => Freshen (a, b) (a', b') where
    freshen (x, y) = (,) <$> freshen x <*> freshen y

instance (Freshen a a', Freshen b b', Freshen c c') => Freshen (a, b, c) (a', b', c') where
    freshen (x, y, z) = (,,) <$> freshen x <*> freshen y <*> freshen z

instance (Freshen a a', Freshen b b', Freshen c c', Freshen d d') => Freshen (a, b, c, d) (a', b', c', d') where
    freshen (x, y, z, w) = (,,,) <$> freshen x <*> freshen y <*> freshen z <*> freshen w

instance (Freshen a a', Freshen b b', Freshen c c', Freshen d d', Freshen e e') => Freshen (a, b, c, d, e) (a', b', c', d', e') where
    freshen (x, y, z, w, a) = (,,,,) <$> freshen x <*> freshen y <*> freshen z <*> freshen w <*> freshen a

data Range = Int :.. Int
           | RMin Int 
           | RMax Int
           deriving (Show, Eq, Generic, Data)

rminmax :: Range -> These Int Int
rminmax (x :.. y) = These x y
rminmax (RMin x)  = This x
rminmax (RMax y)  = That y

instance Semigroup Range where
    (x :.. y) <> (x' :.. y') = min x x' :.. max y y'
    (x :.. y) <> RMin x'     = min x x' :.. y
    (x :.. y) <> RMax y'     = x :.. max y y'
    RMin x    <> (x' :.. y') = min x x' :.. y'
    RMin x    <> RMin x'     = RMin (min x x')
    RMin x    <> RMax y'     = x :.. y'
    RMax y    <> (x' :.. y') = x' :.. max y y'
    RMax y    <> RMin x'     = x' :.. y
    RMax y    <> RMax y'     = RMax (max y y')
