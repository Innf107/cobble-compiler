{-#LANGUAGE UndecidableInstances#-}
module Language.Cobble.Codegen.Common (Range(..), Objective) where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Util.TypeUtils
import Language.Cobble.Util.Polysemy.Fresh
import Language.Haskell.TH (Extension(UndecidableInstances))
import Relude (Semigroup)

type Objective = Text

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
