{-# LANGUAGE TemplateHaskell #-}

module Cobble.Syntax.QualifiedName (
    QualifiedName (..),
    QNameSpecifier (..),
    UnqualifiedName,
    renderDebug,
    renderMinecraft,
    renderLua,
    renderRacket,
    internalQName,
) where

import Cobble.Config (Config (Config), disambiguateModules, disambiguateNames, getConfig)
import Cobble.Prelude

import Cobble.Syntax.LexInfo

import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data

import Data.Text qualified as T

import GHC.Show qualified as S

data QualifiedName = UnsafeQualifiedName
    { originalName :: Text
    , qnameSpecifier :: QNameSpecifier
    }
    deriving (Eq, Ord, Generic, Data)
instance Hashable QualifiedName
instance Binary QualifiedName

data QNameSpecifier
    = GlobalQName Text -- Module
    | LocalQName Int
    | GeneratedQName Int
    deriving (Show, Eq, Ord, Generic, Data)
instance Hashable QNameSpecifier
instance Binary QNameSpecifier

type UnqualifiedName = Text

internalQName :: Text -> QualifiedName
internalQName n = UnsafeQualifiedName n (GlobalQName "Internal")

instance S.Show QualifiedName where
    show = toString . renderDebug

renderDebug :: QualifiedName -> Text
renderDebug (UnsafeQualifiedName name (LocalQName ix)) =
    let Config{disambiguateNames} = getConfig ()
     in if disambiguateNames
            then name <> "_" <> show ix
            else name
renderDebug (UnsafeQualifiedName name (GlobalQName mod)) =
    let Config{disambiguateModules} = getConfig ()
     in if disambiguateModules
            then mod <> "." <> name
            else name
renderDebug (UnsafeQualifiedName name (GeneratedQName ix)) =
    name <> "_" <> show ix

renderMinecraft :: QualifiedName -> Text
renderMinecraft = renameUpper . renameStandardChars . renderDebug
  where
    renameUpper = T.concatMap \case
        c
            | isUpper c -> "-" <> one (toLower c)
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
        '|' -> "?pipe?"
        '#' -> "?hash?"
        ';' -> "?semi?"
        ',' -> "?comma?"
        '\'' -> "?prime?"
        x -> one x

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
