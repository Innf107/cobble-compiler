{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.QualifiedName (
    QualifiedName (..)
,   UnqualifiedName
,   unsafeQualifiedName

,   internalQName

-- TODO
,   Log (..)
,   LogLevel (..)
,   log
) where

import Language.Cobble.Prelude

import Language.Cobble.Types.LexInfo

import Data.Data
import Data.Char
import Data.Generics.Uniplate.Data

import qualified Data.Text as T

import qualified GHC.Show as S

data QualifiedName = ReallyUnsafeQualifiedName {
        originalName :: Text
    ,   renamed :: Text
    ,   location :: LexInfo
    } deriving (Eq, Ord, Generic, Data)
instance Hashable QualifiedName

type UnqualifiedName = Text

internalQName :: Text -> QualifiedName
internalQName n = unsafeQualifiedName n n InternalLexInfo

unsafeQualifiedName :: Text -> Text -> LexInfo -> QualifiedName
unsafeQualifiedName original renamed loc = ReallyUnsafeQualifiedName original (T.concatMap renameChar renamed) loc

instance S.Show QualifiedName where
    show = toString . renamed

renameChar :: Char -> Text
renameChar = \case
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
    x   
        | isUpper x -> "-" <> one (toLower x)
        | otherwise -> one x

-- TODO: Move to a different module
data Log = Log LogLevel Text deriving (Show, Eq, Ord)

data LogLevel = LogWarning     
              | LogInfo        
              | LogVerbose     
              | LogDebug        
              | LogDebugVerbose
              | LogDebugVeryVerbose
              deriving (Show, Eq, Ord, Enum, Read)

log :: (Member (Output Log) r) => LogLevel -> Text -> Sem r ()
log l t = output (Log l t)

