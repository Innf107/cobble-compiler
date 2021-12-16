{-#OPTIONS_GHC -Wno-orphans#-}
module Language.Cobble.Prelude (
      module Relude
    , module Relude.Extra
    , module Polysemy
    , module Polysemy.State
    , module Polysemy.Error
    , module Polysemy.Reader
    , module Polysemy.Writer
    , module Polysemy.Output
    , module Control.Lens
    , module System.FilePath
    , module Data.Generics.Uniplate.Data
    , module Language.Cobble.Util.ListLike
    , module Data.Data
    , module Data.Foldable
    --, module Data.These
    , module Data.Either
    , module Data.Void
    , (|:)
    , state 
    , whenAlt
    , ($$)
    , mapFromLeft
    , munion
    , censorM
    , HSType
    , HSConstraint
    , (L.\\)
    , unsafeLast
    ) where

import qualified Relude
import Relude hiding (
      universe
    , Type
    , Constraint
    , TVar
    , absurd
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
    , zip
    , zipWith
    )
import Language.Cobble.Util.ListLike
    
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
    , lens
    , Lens'
    )

import Polysemy hiding (transform, rewrite)
import Polysemy.State
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Writer hiding (pass)
import Polysemy.Output

import Data.Generics.Uniplate.Data

import System.FilePath

import Data.Either hiding (either)

import Control.Lens hiding (
        universe
    ,   children
    ,   contexts
    ,   holes
    ,   para
    ,   rewriteM
    ,   rewrite
    ,   transformM
    ,   transform
    ,   (<.>)
    ,   op
    )

import Data.Data hiding (Fixity)

import qualified Data.DList as D
import qualified Data.Text as T

import qualified Data.List as L (init, last, (\\))

import qualified Data.Map as M (unionWith)

import Data.Foldable (foldrM)

import Data.Void

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

censorM :: (Member (Writer o) r) => (o -> Sem r ()) -> Sem r a -> Sem r a
censorM f a = do
    (fo, x) <- listen a
    f fo
    pure x

instance Snoc (D.DList a) (D.DList b) a b where
    _Snoc = prism (uncurry D.snoc) (\dl -> case toList dl of
        [] -> Left D.empty
        xs -> Right (fromList (L.init xs), L.last xs)
      )

instance ToText (D.DList Char) where
    toText = toText . toList

instance ToText Char where
    toText = T.singleton
    
type HSType = Relude.Type
    
type HSConstraint = Relude.Constraint

unsafeLast :: [a] -> a
unsafeLast = L.last

munion :: (Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
munion = M.unionWith (<>)
