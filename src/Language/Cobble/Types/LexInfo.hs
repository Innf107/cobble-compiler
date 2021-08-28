module Language.Cobble.Types.LexInfo where

import Language.Cobble.Prelude

type FileName = Text

data LexInfo = LexInfo {
      startPos :: SourcePos
    , endPos :: SourcePos
    , file :: FileName
    } deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Hashable LexInfo

data SourcePos = SourcePos {
      line :: Int
    , column :: Int
    } deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Hashable SourcePos

pattern InternalLexInfo :: LexInfo
pattern InternalLexInfo = LexInfo (SourcePos 0 0) (SourcePos 0 0) "<internal>"


