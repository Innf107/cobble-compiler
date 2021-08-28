{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Types.QualifiedName (
    QualifiedName (..)
,   UnqualifiedName

,   internalQName

-- TODO
,   Log (..)
,   LogLevel (..)
,   log
) where

import Language.Cobble.Prelude

import Language.Cobble.Types.LexInfo

import Data.Data
import Data.Generics.Uniplate.Data

import qualified Data.Text as T

import qualified GHC.Show as S

data QualifiedName = UnsafeQualifiedName {
        originalName :: Text
    ,   renamed :: Text
    ,   location :: LexInfo
    } deriving (Eq, Ord, Generic, Data)
instance Hashable QualifiedName

type UnqualifiedName = Text

internalQName :: Text -> QualifiedName
internalQName n = UnsafeQualifiedName n n InternalLexInfo


instance S.Show QualifiedName where
    show = toString . renamed

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


