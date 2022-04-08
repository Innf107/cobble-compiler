module Cobble.Types.LexInfo where

import Cobble.Prelude

import Text.Show qualified as S

type FileName = Text

data LexInfo = LexInfo {
      startPos :: SourcePos
    , endPos :: SourcePos
    , file :: FileName
    } deriving (Eq, Ord, Generic, Data, Typeable)

instance Hashable LexInfo

data SourcePos = SourcePos {
      line :: Int
    , column :: Int
    } deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Hashable SourcePos

pattern InternalLexInfo :: LexInfo
pattern InternalLexInfo = LexInfo (SourcePos 0 0) (SourcePos 0 0) "<internal>"

instance S.Show LexInfo where
  show (LexInfo{startPos,endPos, file}) = toString $ file <> ":" <> show (line startPos) <> ":" <> show (column startPos) 
                                                          <> "-" <> show (line endPos) <> ":" <> show (column endPos)
