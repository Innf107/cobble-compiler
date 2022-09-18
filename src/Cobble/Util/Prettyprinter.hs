module Cobble.Util.Prettyprinter (
    module Export 
,   vsep, sep, encloseSep
) where

import Cobble.Prelude

import Prettyprinter qualified as P
import Prettyprinter as Export hiding (
        vsep, sep, encloseSep, Pretty(..)
    )

vsep :: Seq (Doc ann) -> Doc ann
vsep = P.vsep . toList

sep :: Seq (Doc ann) -> Doc ann
sep = P.sep . toList

encloseSep :: Doc ann -> Doc ann -> Doc ann -> Seq (Doc ann) -> Doc ann
encloseSep start end sep = P.encloseSep start end sep . toList
