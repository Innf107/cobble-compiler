{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | This module contains behaviour and types that are shared between
-- the Cobble and MCAsm Compiler
module Language.Cobble.Shared where

import Language.Cobble.Prelude

import qualified Data.Text as T

import qualified GHC.Show as S

data QualifiedName = QualifiedName {qualComponents::[Text]} deriving (Eq, Ord)

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
makeQName = QualifiedName . T.split (=='.')