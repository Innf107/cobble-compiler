{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Cobble.Util.Maybe where

import Language.Cobble.Prelude

withDefault :: Maybe a -> a -> a
withDefault = flip fromMaybe

(?.) :: Maybe a -> a -> a
(?.) = withDefault

infix 0 ?.
