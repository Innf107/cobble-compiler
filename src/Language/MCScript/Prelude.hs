{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, MultiParamTypeClasses, BlockArguments, LambdaCase#-}
{-# LANGUAGE OverloadedStrings #-}

module Language.MCScript.Prelude (
      module Relude
    , module Relude.Extra
    , module Polysemy
    , module Polysemy.State
    , module Polysemy.Error
    , module Polysemy.Reader
    , module Polysemy.Writer
    , module Control.Lens
    , (|:)
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

import Control.Lens

(|:) :: a -> NonEmpty a -> NonEmpty a
a |: (x :| xs) = a :| (x : xs)

