module Cobble.Util.Maybe where

import Cobble.Prelude

withDefault :: Maybe a -> a -> a
withDefault = flip fromMaybe

(?.) :: Maybe a -> a -> a
(?.) = withDefault

infix 0 ?.
