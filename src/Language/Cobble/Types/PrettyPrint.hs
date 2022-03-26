{-# LANGUAGE UndecidableInstances #-}
module Language.Cobble.Types.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.Parser.Tokenizer (Token(..), TokenData(..))
import Language.Cobble.Util.Maybe

import qualified Data.Text as T

prettyPrintToken :: (IsString s) => Token -> s 
prettyPrintToken = fromString . prettyPrintTokenInner
    where
        prettyPrintTokenInner :: Token -> String
        prettyPrintTokenInner (Token _ d) = case d of
            Ident i -> toString i
            Reserved r -> toString r
            Paren p -> toString p
            Operator o -> toString o
            ReservedOp o -> toString o
            IntLiteral l -> show l
            Dedent -> "dedent"
            BlockEnd -> "end of block"

