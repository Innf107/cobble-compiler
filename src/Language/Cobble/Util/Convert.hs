{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Cobble.Util.Convert where

import Language.Cobble.Prelude

class Convert a b where
    conv :: a -> b

instance (Functor f, Convert a b) => Convert (f a) (f b) where
    conv = fmap conv

convInt :: (Integral a, Num b) => a -> b
convInt = fromIntegral . toInteger
