{-# LANGUAGE NoImplicitPrelude #-}
module Language.Cobble.Types.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.Parser.Tokenizer (Token(..), TokenData(..))

prettyPrintToken :: (IsString s) => Token a -> s 
prettyPrintToken = fromString . prettyPrintTokenInner
    where
        prettyPrintTokenInner :: Token a -> String
        prettyPrintTokenInner (Token li d) = case d of
            Ident i -> toString i
            Reserved r -> toString r
            Paren p -> toString p
            Operator o -> toString o
            ReservedOp o -> toString o
            IntLiteral l -> show l
            MacroCall m -> toString m
        