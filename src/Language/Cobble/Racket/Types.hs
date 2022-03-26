module Language.Cobble.Racket.Types where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName

import Prettyprinter as P hiding (list)
import Data.Text qualified as T

data RacketExpr =
    RDefine QualifiedName RacketExpr
  | RDefineF QualifiedName (Seq QualifiedName) (Seq RacketExpr)
  | RVar QualifiedName
  | RApp RacketExpr (Seq RacketExpr)
  | RLambda (Seq QualifiedName) (Seq RacketExpr)
  | RLet (Seq (QualifiedName, RacketExpr)) (Seq RacketExpr)
  | RBegin (Seq RacketExpr)

  | RSymbol QualifiedName
  | RIntLit Int
  | RTrue
  | RFalse
  | RNil
  | RHash (Seq (RacketExpr, RacketExpr))
  | RList (Seq RacketExpr)

  | RAdd (Seq RacketExpr)
  | RSub (Seq RacketExpr)
  | RMul (Seq RacketExpr)
  | RMod (Seq RacketExpr)
  | RQuotient (Seq RacketExpr)
  | RLE  (Seq RacketExpr)
  | REQ  (Seq RacketExpr)
  | RIf RacketExpr RacketExpr RacketExpr

  | RHashSet RacketExpr RacketExpr RacketExpr
  | RHashRef RacketExpr RacketExpr
  | RCadr Int RacketExpr -- RCadr n e ==> (cad{n}r e)  
  
  | RCase RacketExpr (Seq (RPattern, RacketExpr))

  | RDisplayln RacketExpr
  deriving (Show, Eq)

data RPattern = RIntP (Seq Int)
              | RWildcardP
              deriving (Show, Eq)

instance {-# OVERLAPPING #-} Pretty (Seq RacketExpr) where
    pretty = vsep . toList . map pretty

instance Pretty RacketExpr where
    pretty (RDefine x e) = list ["define", prettyQ x, nest 4 (pretty e)]
    pretty (RDefineF f xs es) = parens ("define" <+> list (prettyQ f <| map prettyQ xs) <> nest 4 (line <> pretty es))
    pretty (RVar x) = prettyQ x
    pretty (RApp f xs) = prettyApp (pretty f) (map pretty xs) 
    pretty (RLambda xs body) = parens ("lambda" <+> list (map prettyQ xs) <+> nest 4 (pretty body))
    pretty (RLet bindings body) = parens ("let*" <+> brackets (align (vsep (toList (map prettyBinding bindings)))) <+> nest 4 (line <> pretty body))
    pretty (RBegin es) = parens ("begin" <> nest 4 (line <> pretty es))
    pretty (RSymbol x) = "'" <> prettyQ x
    pretty (RIntLit x) = pretty x
    pretty RTrue = "#t"
    pretty RFalse = "#f"
    pretty RNil = "'()"
    pretty (RHash args) = prettyApp "hash" (concatMap (\(x, e) -> [pretty x, pretty e]) args)
    pretty (RList args) = prettyApp "list" (map pretty args)
    pretty (RAdd args)   = prettyApp "+" (map pretty args)
    pretty (RSub args)   = prettyApp "-" (map pretty args)
    pretty (RMul args)   = prettyApp "*" (map pretty args)
    pretty (RMod args)   = prettyApp "modulo" (map pretty args)
    pretty (RQuotient args) = prettyApp "quotient" (map pretty args)
    pretty (RLE args)    = prettyApp "<=" (map pretty args)
    pretty (REQ args)    = prettyApp "=" (map pretty args)
    pretty (RIf c th el) = parens ("if" <+> pretty c <+> (nest 4 (line <> align (vsep [pretty th, pretty el]))))
    pretty (RHashSet h k e) = prettyApp "hash-set" [pretty h, pretty k, pretty e]
    pretty (RHashRef h k) = prettyApp "hash-ref" [pretty h, pretty k]
    pretty (RCadr n e) = prettyApp (pretty $ "ca" <> T.replicate n "d" <> "r") [pretty e]
    pretty (RCase expr cases) = prettyApp ("case" <+> pretty expr) $
        map (\(pat, e) -> list [prettyPat pat, pretty e]) cases
        where
            prettyPat RWildcardP = "else"
            prettyPat (RIntP is) = list (map pretty is) 
    pretty (RDisplayln expr) = prettyApp "displayln" [pretty expr]

prettyQ :: QualifiedName -> Doc ann
prettyQ = pretty . renderRacket

prettyBinding :: (QualifiedName, RacketExpr) -> Doc ann
prettyBinding (x, e) = parens (prettyQ x <+> nest 4 (pretty e))

prettyApp :: Doc ann -> (Seq (Doc ann)) -> Doc ann 
prettyApp f xs = parens (f <+> align (sep (toList xs)))

list :: (Seq (Doc ann)) -> Doc ann
list = parens . hsep . toList

prettyRacketWithRuntime :: Seq RacketExpr -> Doc ann
prettyRacketWithRuntime body = header <> line <> line <> pretty body <> line <> line <> footer
    where
        header = "#lang racket"
        footer = ""
