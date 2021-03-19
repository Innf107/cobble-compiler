{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, MultiParamTypeClasses, BlockArguments, LambdaCase#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Cobble.Prelude (
      module Relude
    , module Relude.Extra
    , module Polysemy
    , module Polysemy.State
    , module Polysemy.Error
    , module Polysemy.Reader
    , module Polysemy.Writer
    , module Control.Lens
    , module System.FilePath
    , (|:)
    , state 
    , whenAlt
    , ($$)
    , mapFromLeft
    ) where

import Relude hiding (
      Type
    , ask
    , asks
    , init
    , get
    , gets
    , modify
    , modify'
    , put
    , State
    , state
    , evalState
    , runState
    , execState
    , fromException
    , Reader
    , local
    , runReader
    , uncons
    , (??)
    )
import Relude.Extra hiding (
      last1
    , head1
    , under
    , un
    , view
    , set
    , over
    , (^.)
    , (.~)
    , (%~)
    , universe
    , lens
    , Lens'
    )

import Polysemy hiding (transform, rewrite)
import Polysemy.State
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Writer hiding (pass)


import System.FilePath hiding ((<.>))

import Control.Lens

(|:) :: a -> NonEmpty a -> NonEmpty a
a |: (x :| xs) = a :| (x : xs)


state :: (Member (State s) r) => (s -> (a, s)) -> Sem r a
state f = get >>= \(f -> (r, s')) -> put s' *> pure r
    

whenAlt :: (Alternative f) => Bool -> a -> f a
whenAlt b x = if b then pure x else empty

($$) :: (a -> b) -> a -> b
($$) = ($)

infixr 5 $$

mapFromLeft :: (a -> b) -> Either a b -> b
mapFromLeft = (`either`id)
