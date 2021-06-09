module Language.Cobble.Util.TypeUtils where

import Language.Cobble.Prelude

type AllC :: (k -> Constraint) -> [k] -> Constraint
type family AllC c xs where
    AllC c '[] = ()
    AllC c (x : xs) = (c x, AllC c xs)

type AllOnPass :: (k2 -> Constraint) -> k1 -> [(k1 -> k2)] -> Constraint
type family AllOnPass c p xs where
    AllOnPass c p '[] = ()
    AllOnPass c p (x : xs) = (c (x p), AllOnPass c p xs)

data DictDisj c1 c2 where
    DictLeft :: c1 => DictDisj c1 c2
    DictRight :: c2 => DictDisj c1 c2
