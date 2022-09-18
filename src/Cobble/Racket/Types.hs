module Cobble.Racket.Types where

import Cobble.Prelude
import Cobble.Syntax.QualifiedName

import Prettyprinter as P hiding (list, Pretty(..))
import Data.Text qualified as T

data RacketExpr =
    RDefine QualifiedName RacketExpr
  | RDefineF QualifiedName (Seq QualifiedName) (Seq RacketExpr)
  | RVar QualifiedName
  | RBuiltin Text
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


instance Pretty RacketExpr where
    prettyPrec _ (RDefine x e) = list ["define", prettyQ x, nest 4 (pretty e)]
    prettyPrec _ (RDefineF f xs es) = parens ("define" <+> list (prettyQ f <| map prettyQ xs) <> nest 4 (line <> prettyRacketExprs es))
    prettyPrec _ (RVar x) = prettyQ x
    prettyPrec _ (RBuiltin x) = pretty x
    prettyPrec _ (RApp f xs) = prettyApp (pretty f) (map pretty xs) 
    prettyPrec _ (RLambda xs body) = parens ("lambda" <+> list (map prettyQ xs) <+> nest 4 (prettyRacketExprs body))
    prettyPrec _ (RLet bindings body) = parens ("let*" <+> brackets (align (vsep (toList (map prettyBinding bindings)))) <+> nest 4 (line <> prettyRacketExprs body))
    prettyPrec _ (RBegin es) = parens ("begin" <> nest 4 (line <> prettyRacketExprs es))
    prettyPrec _ (RSymbol x) = "'" <> prettyQ x
    prettyPrec _ (RIntLit x) = pretty x
    prettyPrec _ RTrue = "#t"
    prettyPrec _ RFalse = "#f"
    prettyPrec _ RNil = "'()"
    prettyPrec _ (RHash args) = prettyApp "hash" (concatMap (\(x, e) -> [pretty x, pretty e]) args)
    prettyPrec _ (RList args) = prettyApp "list" (map pretty args)
    prettyPrec _ (RAdd args)   = prettyApp "+" (map pretty args)
    prettyPrec _ (RSub args)   = prettyApp "-" (map pretty args)
    prettyPrec _ (RMul args)   = prettyApp "*" (map pretty args)
    prettyPrec _ (RMod args)   = prettyApp "modulo" (map pretty args)
    prettyPrec _ (RQuotient args) = prettyApp "quotient" (map pretty args)
    prettyPrec _ (RLE args)    = prettyApp "<=" (map pretty args)
    prettyPrec _ (REQ args)    = prettyApp "=" (map pretty args)
    prettyPrec _ (RIf c th el) = parens ("if" <+> pretty c <+> (nest 4 (line <> align (vsep [pretty th, pretty el]))))
    prettyPrec _ (RHashSet h k e) = prettyApp "hash-set" [pretty h, pretty k, pretty e]
    prettyPrec _ (RHashRef h k) = prettyApp "hash-ref" [pretty h, pretty k]
    prettyPrec _ (RCadr n e) = prettyApp (pretty $ "ca" <> T.replicate n "d" <> "r") [pretty e]
    prettyPrec _ (RCase expr cases) = prettyApp ("case" <+> pretty expr) $
        map (\(pat, e) -> list [prettyPat pat, pretty e]) cases
        where
            prettyPat RWildcardP = "else"
            prettyPat (RIntP is) = list (map pretty is) 
    prettyPrec _ (RDisplayln expr) = prettyApp "displayln" [pretty expr]

prettyQ :: QualifiedName -> Doc ann
prettyQ = pretty . renderRacket

prettyBinding :: (QualifiedName, RacketExpr) -> Doc ann
prettyBinding (x, e) = parens (prettyQ x <+> nest 4 (pretty e))

prettyApp :: Doc ann -> (Seq (Doc ann)) -> Doc ann 
prettyApp f xs = parens (f <+> align (sep (toList xs)))

list :: (Seq (Doc ann)) -> Doc ann
list = parens . hsep . toList

prettyRacketExprs :: Seq RacketExpr -> Doc ann
prettyRacketExprs = vsep . toList . map pretty
