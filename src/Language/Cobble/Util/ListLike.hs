{-# LANGUAGE QuantifiedConstraints #-}
module Language.Cobble.Util.ListLike where

import GHC.Exts
import Relude (Functor(..), uncurry, const, (.), id)
import qualified Relude as R
import qualified Data.List.NonEmpty as N
import qualified Data.Sequence as S

class (Functor l) => ListLike l where
    mkList :: [a] -> l a

    zipWith :: (a -> b -> c) -> l a -> l b -> l c
    zipWith f = (fmap (uncurry f) .) . zip
    zip :: l a -> l b -> l (a, b)
    zip = zipWith (,)

    zipWithIndex :: (Int -> a -> b) -> l a -> l b
    zipWithIndex f = fmap (uncurry f) . zipIndex
    zipIndex :: l a -> l (Int, a)
    zipIndex = zip (mkList [0..])
    indexes :: l a -> l Int
    indexes = zipWithIndex const
    {-# MINIMAL mkList, (zip | zipWith), (zip | zipWith | zipWithIndex | zipIndex) #-}

instance ListLike [] where
    mkList  = id
    zipWith = R.zipWith
    zip     = R.zip

instance ListLike R.NonEmpty where
    mkList  = fromList
    zipWith = N.zipWith
    zip     = N.zip


instance ListLike S.Seq where
    mkList = fromList
    zipWith = S.zipWith
    zip = S.zip
