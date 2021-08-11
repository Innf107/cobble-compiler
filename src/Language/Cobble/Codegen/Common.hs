{-#LANGUAGE UndecidableInstances#-}
module Language.Cobble.Codegen.Common (freshVar, freshen, Range(..), Objective) where

import Language.Cobble.Prelude
import Language.Cobble.Shared
import Language.Cobble.Util.TypeUtils
import Language.Haskell.TH (Extension(UndecidableInstances))
import Relude (Semigroup)

type Objective = Text

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
           | RLE Int 
           | RGE Int
           | REQ Int
           deriving (Show, Eq, Generic, Data)

instance Semigroup Range where
    (x :.. y) <> (x' :.. y') = min x x' :.. max y y'
    (x :.. y) <> RGE x'      = min x x' :.. y
    (x :.. y) <> RLE y'      = x :.. max y y'
    RGE x     <> (x' :.. y') = min x x' :.. y'
    RGE x     <> RGE x'      = RGE (min x x')
    RGE x     <> RLE y'      = x :.. y'
    RLE y     <> (x' :.. y') = x' :.. max y y'
    RLE y     <> RGE x'      = x' :.. y
    RLE y     <> RLE y'      = RLE (max y y')
    x         <> REQ y       = x <> (y :.. y)
    (REQ x)   <> y           = (x :.. x) <> y
