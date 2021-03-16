{-# LANGUAGE NoImplicitPrelude, DataKinds, LambdaCase, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, KindSignatures, RankNTypes, ScopedTypeVariables#-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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

class PrettyPrint x where
    prettyPrint :: x -> Text

instance PrettyPrint Token where prettyPrint = prettyPrintToken

instance PrettyPrint Text where prettyPrint = id

instance PrettyPrint QualifiedName where prettyPrint = show

instance (PrettyPrintExt p, PrettyPrint (Name p), Show (Statement p), Show (Expr p))
    => PrettyPrint (Statement p) where
    prettyPrint = \case
        s@(CallFun _ _ n args) -> prettyPrintExtSt s
                    ?. prettyPrint n <> " (" <> T.intercalate "," (map prettyPrint args) <> ")"
        s@(DefVoid _ _ n ps b) -> prettyPrintExtSt s
                    ?. "void " <> prettyPrint n <> "(" <>
                    T.intercalate ","  (map (\(pn, pt) -> prettyPrint pn <> ": " <> prettyPrint pt) ps) <> ")"
                    <> "\n{\n" <> T.unlines (map prettyPrint b) <> "\n}"
        s@(DefFun _ _ n ps b r t) -> prettyPrintExtSt s
                    ?. prettyPrint t <> " " <> prettyPrint n <> " ("
                    <> T.intercalate "," (map (\(pn, pt) -> prettyPrint pn <> ": " <> prettyPrint pt) ps)
                    <> ")"
                    <> "\n{\n" <> T.unlines (map prettyPrint b) <> "\n} => " <> prettyPrint r
        s@(Decl _ _ n mt e) -> prettyPrintExtSt s
                    ?. "let " <> prettyPrint n <> (maybe "" (\t -> ": " <> prettyPrint t) mt) <> " = " <> prettyPrint e
        s@(Assign _ _ n e) -> prettyPrintExtSt s
                    ?. prettyPrint n <> " = " <> prettyPrint e
        s@(While _ _ c b) -> prettyPrintExtSt s
                    ?. "while (" <> prettyPrint c <> ")\n{\n" <> T.unlines (map prettyPrint b) <> "\n}"
        s@(DefStruct _ _ n ts) -> prettyPrintExtSt s
                    ?. "struct " <> prettyPrint n <> "\n{\n" <> T.unlines (map (\(tn, tt) -> prettyPrint tn <> ": " <> prettyPrint tt) ts)
                    <> "\n}\n"
        s -> prettyPrintExtSt s ?. (error $ "Cannot PrettyPrint '" <> show s <> "'")

instance (PrettyPrintExt p, PrettyPrint (Name p), Show (Expr p)) => PrettyPrint (Expr p) where
    prettyPrint = \case
        e@(IntLit _ _ i) -> prettyPrintExtEx e ?. show i
        e@(BoolLit _ _ b) -> prettyPrintExtEx e ?. bool "false" "true" b
        e@(FCall _ _ n as) -> prettyPrintExtEx e ?. prettyPrint n <> "(" <> T.intercalate ", " (map prettyPrint as) <> ")"
        e@(Var _ _ n) -> prettyPrintExtEx e ?. prettyPrint n
        e -> prettyPrintExtEx e ?. error ("Cannot PrettyPrint '" <> show e <> "'")

instance (PrettyPrint (Name p)) => PrettyPrint (Type p) where
    prettyPrint = \case
        IntT -> "int"
        BoolT -> "bool"
        EntityT -> "entity"
        StructT n -> prettyPrint n

class PrettyPrintExt (p :: Pass) where
    prettyPrintExtSt :: Statement p -> Maybe Text
    prettyPrintExtEx :: Expr p -> Maybe Text


instance PrettyPrintExt 'QualifyNames where
    prettyPrintExtSt _ = Nothing
    prettyPrintExtEx _ = Nothing

instance PrettyPrintExt 'Typecheck where
    prettyPrintExtSt _ = Nothing
    prettyPrintExtEx _ = Nothing

instance PrettyPrintExt 'Codegen where
    prettyPrintExtSt _ = Nothing
    prettyPrintExtEx = \case
        Var t _ n -> Just $ prettyPrint n <> " {-: " <> prettyPrint t <> "-}"
        FCall t _ n ps -> Just $ prettyPrint n <> "(" <> T.intercalate ", " (map prettyPrint ps) <> ") {-: "
                            <> prettyPrint t <> "-}"
        _ -> Nothing
