{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Codegen.PrimOps where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Types

import Data.Map qualified as M
import qualified GHC.Exts

data PrimOp = True_
            | False_
            | Add
            | Sub
            | Mul
            | Div
            | Mod
            | LE
            | SetTestScoreboardUnsafe
            deriving (Show, Eq, Generic, Data)

data PrimOpInfo = PrimOpInfo {
        _primOp :: PrimOp
    ,   _primOpType :: Type Codegen
    }

primOps :: Map QualifiedName PrimOpInfo 
primOps = M.mapKeys (\k -> internalQName k) $ fromList [
        ("__true__", PrimOpInfo True_ (unitT -:> boolT))
    ,   ("__false__", PrimOpInfo False_ (unitT -:> boolT))
    ,   ("__add__", PrimOpInfo Add (intT -:> intT -:> intT))
    ,   ("__sub__", PrimOpInfo Sub (intT -:> intT -:> intT))
    ,   ("__mul__", PrimOpInfo Mul (intT -:> intT -:> intT))
    ,   ("__div__", PrimOpInfo Div (intT -:> intT -:> intT))
    ,   ("__mod__", PrimOpInfo Mod (intT -:> intT -:> intT))
    ,   ("__le__",  PrimOpInfo LE  (intT -:> intT -:> boolT))
    ,   ("__setTestScoreboardUnsafe__", PrimOpInfo SetTestScoreboardUnsafe (intT -:> unitT))
    ]

makeLenses ''PrimOpInfo 