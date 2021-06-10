-- | This module contains behaviour and types that are shared between
-- the Cobble and MCAsm Compiler
module Language.Cobble.Shared where

import Language.Cobble.Prelude

import Data.Data
import Data.Generics.Uniplate.Data

import qualified Data.Text as T

import qualified GHC.Show as S

data QualifiedName = QualifiedName {qualComponents::[Text]} deriving (Eq, Ord, Generic, Data, Typeable)

type UnqualifiedName = Text

instance S.Show QualifiedName where
    show = toString . T.intercalate "." . qualComponents

prependQual :: Text -> QualifiedName -> QualifiedName
prependQual t (QualifiedName cs) = QualifiedName $ t:cs

instance Semigroup QualifiedName where
    QualifiedName cs1 <> QualifiedName cs2 = QualifiedName (cs1 <> cs2)
    
instance Monoid QualifiedName where
    mempty = QualifiedName []
    
instance IsString QualifiedName where
    fromString = makeQName . toText

(.:) :: QualifiedName -> Text -> QualifiedName
(QualifiedName cs) .: c = QualifiedName (cs <> [c]) 

makeQName :: Text -> QualifiedName
makeQName "" = QualifiedName []
makeQName t  = QualifiedName $ T.split (=='.') t

unqualifyName :: QualifiedName -> UnqualifiedName
unqualifyName (QualifiedName ps) = fromMaybe "" $ viaNonEmpty last ps

data Panic = Panic Text 
           | ModuleDependencyNotFound Text
           | MismatchedRegTypes Text Text
           deriving (Show, Eq)

panic :: (Member (Error Panic) r) => Text -> Sem r a
panic = throw . Panic

panic' :: (Member (Error Panic) r) => Text -> [Text] -> Sem r a
panic' t as = panic $ t <> "\n\nContext: \n" <> unlines (map ("    "<>) as)

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

data Target = Target {
    markerType :: MarkerType
,   packFormat :: Int
} deriving (Show, Eq)

target116, target117 :: Target
target116 = Target {
    markerType = MarkerCloud
,   packFormat = 6
}
target117 = Target {
    markerType = MarkerEntity
,   packFormat = 7
}


data MarkerType = MarkerEntity | MarkerCloud deriving (Show, Eq)

