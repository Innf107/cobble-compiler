{-#OPTIONS_GHC -Wno-orphans#-}
module Cobble.Prelude (
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
    , module Cobble.Util.ListLike
    , module Cobble.Util.Trace
    , module Data.Data
    , module Data.Foldable
    , module Data.Sequence
    , module Data.Either
    , module Data.Void
    , module Prettyprinter
    , module Witherable
    , (|:)
    , map
    , concatMap
    , (\\)
    , diffEq
    , concat
    , runOutputSeq
    , intercalate
    , state 
    , whenAlt
    , mapFromLeft
    , munion
    , censorM
    , HSType
    , HSConstraint
    , unsafeLast
    , Binary
    , lookupSeq
    , unzip3
    , Pretty(..)
    , Prec(..)
    , prettyParen
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
    , zipWith, zipWithM, zipWithM_
    , trace, traceM, traceId, traceShow, traceShowId, traceShowM, traceShowWith
    , unfoldr
    , map, concatMap, concat, tails, inits, sortOn, sort, intersperse, zip3, splitAt, scanr, scanr1, scanl, scanl1
    , replicateM, take, reverse, replicate, drop, length, sortBy, zip, unzip, unzip3, null
    , mapMaybe, mapMaybeM, catMaybes
    , ordNub, ordNubOn, hashNub
    , filter
    , intercalate
    )
import Cobble.Util.ListLike
import Cobble.Util.Trace

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
    , (!?)
    , lens
    , Lens'
    )

import Polysemy hiding (transform, rewrite, Effect)
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
    ,   (|>), (<|), (:>), (:<), Empty
    ,   Context
    )

import Data.Data hiding (Fixity)

import qualified Data.DList as D
import qualified Data.Text as T

import qualified Data.List as L (init, last, head, (\\))

import qualified Data.Map as M (unionWith)

import Data.Foldable (foldrM)

import Data.Void

import Data.Sequence hiding (
        empty, fromList, lookup, insertAt, index, zip, zipWith, filter
    )
import Witherable

import Data.Binary (Binary)

import Prettyprinter (Doc)
import Prettyprinter qualified as P

(|:) :: a -> NonEmpty a -> NonEmpty a
a |: (x :| xs) = a :| (x : xs)

map :: (a -> b) -> Seq a -> Seq b
map = fmap

concatMap :: (Foldable t) => (a -> Seq b) -> t a -> Seq b
concatMap = foldMap

-- | O(nlog(n))
(\\) :: forall a. (Ord a) => Seq a -> Seq a -> Seq a
s1 \\ s2 = filter (`notMember` s2Set) s1
    where
        s2Set :: Set a
        s2Set = fromList (toList s2)

-- | O(n^2)
diffEq :: Eq a => Seq a -> Seq a -> Seq a
diffEq s1 s2 = filter (`notElem` s2) s1

concat :: Foldable t => t (Seq a) -> Seq a
concat = fold

runOutputSeq :: Sem (Output o : r) a -> Sem r (Seq o, a)
runOutputSeq = fmap (first fromList) . runOutputList

intercalate :: Text -> Seq Text -> Text
intercalate sep seq = T.intercalate sep (toList seq)

state :: (Member (State s) r) => (s -> (a, s)) -> Sem r a
state f = get >>= \(f -> (r, s')) -> put s' *> pure r
    
whenAlt :: (Alternative f) => Bool -> a -> f a
whenAlt b x = if b then pure x else empty

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

unsafeLast :: HasCallStack => Seq a -> a
unsafeLast (_ :|> x) = x
unsafeLast _ = error $ "unsafeLast: empty list"

munion :: (Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
munion = M.unionWith (<>)

lookupSeq :: Eq k => k -> Seq (k, v) -> Maybe v
lookupSeq _k Empty = Nothing
lookupSeq k ((k', v) :<| rest)
    | k == k'   = Just v
    | otherwise = lookupSeq k rest

unzip3 :: Seq (a, b, c) -> (Seq a, Seq b, Seq c)
unzip3 Empty = ([], [], [])
unzip3 ((x, y, z) :<| ts) = let (xs, ys, zs) = unzip3 ts in
                            (x <| xs, y <| ys, z <| zs)


data Prec = AtomPrec | AppPrec | FunPrec | SigPrec | LetPrec deriving (Show, Eq, Enum, Ord, Bounded)

class Pretty a where
    prettyPrec :: Prec -> a -> Doc ann
    pretty :: a -> Doc ann
    pretty = prettyPrec LetPrec

prettyParen :: Prec -> Prec -> Doc ann -> Doc ann
prettyParen p1 p2 x
    | p1 < p2   = "(" <> x <> ")"
    | otherwise = x

instance Pretty Int where prettyPrec _ = P.pretty
instance Pretty Text where prettyPrec _ = P.pretty
instance Pretty Bool where prettyPrec _ = P.pretty
