module Cobble.Interface where

import Cobble.Prelude
import Cobble.Types as C
import Cobble.Core.Types as F

import Data.Binary

data Interface = Interface {
    interfaceModName :: Text
,   interfaceModSig :: ModSig
,   interfaceCoreModSig :: CoreModSig
} deriving (Show, Eq, Generic, Data)

instance Binary Interface

data CoreModSig = CoreModSig {
    coreModVars :: Map QualifiedName F.Type
,   coreModDictTyDefs :: Map QualifiedName (Seq (QualifiedName, F.Kind), Seq (QualifiedName, F.Type)) 
} deriving (Show, Eq, Generic, Data)

instance Binary CoreModSig

emptyCoreModSig :: CoreModSig
emptyCoreModSig = CoreModSig mempty mempty

extractCoreSig :: Seq F.Decl -> CoreModSig
extractCoreSig decls = foldr (mergeCoreModSig . extractPartialCoreSig) emptyCoreModSig decls

mergeCoreModSig :: CoreModSig -> CoreModSig -> CoreModSig
mergeCoreModSig (CoreModSig mv1 dd1) (CoreModSig mv2 dd2) = CoreModSig (mv1 <> mv2) (dd1 <> dd2)

extractPartialCoreSig :: F.Decl -> CoreModSig
extractPartialCoreSig (F.Def x ty e) = emptyCoreModSig {coreModVars = one (x, ty)}
-- TODO: This is literally copy-pasted from Core Lint, we should probably find a way to unify the two.
extractPartialCoreSig (F.DefVariant x args clauses) = emptyCoreModSig {
        coreModVars = let resKind = foldr (F.KFun . snd) F.KType args
                          resTy = foldl' (F.TApp) (F.TCon x resKind) (map (uncurry F.TVar) args)
                        -- TODO: We have to include effect variables in the function arrows
                      in undefined -- fromList $ toList $ clauses <&> \(constr, constrArgs) -> (constr, (foldr (uncurry F.TForall) (foldr F.TFun resTy constrArgs) args))
    }
extractPartialCoreSig (F.DefDict x args fields) = emptyCoreModSig {coreModDictTyDefs = one (x, (args, fields))}
extractPartialCoreSig (F.DefEffect x args ops) = error "TODO: extract F.DefEffect NYI" 
