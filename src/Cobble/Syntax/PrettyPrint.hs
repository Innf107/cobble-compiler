{-# LANGUAGE UndecidableInstances #-}
module Cobble.Syntax.PrettyPrint where

import Cobble.Prelude
import Cobble.Syntax
import Cobble.Parser.Tokenizer (Token(..), TokenData(..))
import Cobble.Util.Maybe

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

