{-#LANGUAGE TemplateHaskell#-}
module Cobble.Types.QualifiedName (
    QualifiedName (..)
,   UnqualifiedName
,   renderDebug
,   renderMinecraft
,   renderLua
,   renderRacket

,   internalQName
) where

import Cobble.Prelude

import Cobble.Types.LexInfo

import Data.Data
import Data.Char
import Data.Generics.Uniplate.Data

import qualified Data.Text as T

import qualified GHC.Show as S

data QualifiedName = UnsafeQualifiedName {
        originalName :: Text
    ,   qnameIndex :: Int
    ,   location :: LexInfo
    } deriving (Eq, Ord, Generic, Data)
instance Hashable QualifiedName
instance Binary QualifiedName

type UnqualifiedName = Text

internalQName :: Text -> QualifiedName
internalQName n = UnsafeQualifiedName n 0 InternalLexInfo

instance S.Show QualifiedName where
    show = toString . renderDebug

renderDebug :: QualifiedName -> Text
renderDebug (UnsafeQualifiedName name ix _) = name <> "_" <> show ix

renderMinecraft :: QualifiedName -> Text
renderMinecraft = renameUpper . renameStandardChars . renderDebug
    where
        renameUpper = T.concatMap \case
            c | isUpper c -> "-" <> one (toLower c)
              | otherwise -> one c

renderLua :: QualifiedName -> Text
renderLua = renameMinus . renameStandardChars . renderDebug
    where
        renameMinus = T.concatMap \case
            '-' -> "__"
            x -> one x

renderRacket :: QualifiedName -> Text
renderRacket = renameRacketChars . renderDebug
    where
        renameRacketChars = T.concatMap \case
            '.' -> "?dot?"
            '|' -> "?pipe?"
            '#' -> "?hash?"
            ';' -> "?semi?"
            ',' -> "?comma?"
            x   -> one x



renameStandardChars :: Text -> Text
renameStandardChars = T.concatMap \case
    '+' -> "-plus"
    '-' -> "-minus"
    '*' -> "-star"
    '/' -> "-slash"
    '~' -> "-tilde"
    '^' -> "-circ"
    '!' -> "-excl"
    '?' -> "-quest"
    '.' -> "-dot"
    '|' -> "-pipe"
    '<' -> "-lt"
    '>' -> "-gt"
    '$' -> "-dollar"
    '&' -> "-amp"
    '=' -> "-eq"
    '#' -> "-hash"
    ':' -> "-col"
    ';' -> "-semi"
    ',' -> "-comma"
    x -> one x

