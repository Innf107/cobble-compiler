{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Types.TH where

import Relude hiding (Type, last)-- Not importing Language.Cobble.Prelude to avoid unnecessary name conflicts
import Relude.Extra

import Data.List (nub, last)

import Language.Haskell.TH

import Language.Cobble.Util.TypeUtils
import Data.Data (Data)

derivePass :: Bool -> Name -> Name -> Q [Dec]
derivePass withArg className typeName = do
    let class_ = conT className
    let type_ = conT typeName
    let allC  = conT ''AllC
    let instanceReq = conT $ mkName "InstanceRequirements"
    if withArg
    then [d|deriving instance ($allC $class_ ($instanceReq ($type_ p))) => $class_ ($type_ p)|]
    else [d|deriving instance ($allC $class_ ($instanceReq ($type_)))   => $class_ ($type_)|]

derivePassWithConstraint :: Name -> Name -> Name -> Q [Dec]
derivePassWithConstraint constraint className typeName = do
    let class_ = conT className
    let type_ = conT typeName
    let allC  = conT ''AllC
    let instanceReq = conT $ mkName "InstanceRequirements"
    [d|deriving instance ($(conT constraint) p, $allC $class_ ($instanceReq ($type_ p))) => $class_ ($type_ p)|]


derivePassTypeable :: Bool -> Name -> Name -> Q [Dec]
derivePassTypeable withArg = if withArg 
                             then derivePassWithConstraint ''Typeable
                             else derivePass withArg

deriveDefault :: Bool -> Name -> Q [Dec]
deriveDefault withArg ty = concat <$> sequence [
        derivePass withArg ''Show ty
    ,   derivePass withArg ''Eq ty
    ,   derivePassTypeable withArg ''Data ty
    ,   if withArg 
        then [d|deriving instance Generic ($(conT ty) p)|]
        else [d|deriving instance Generic $(conT ty)|]
    ]

deriveInstanceReqs :: Q [Dec]
deriveInstanceReqs = do
    let instanceReqs = mkName "InstanceRequirements"
    (FamilyI _ instances) <- reify instanceReqs
    concat <$> forM instances \(TySynInstD (TySynEqn _ t _))-> do
        case t of
            AppT _ (AppT (ConT cname) (VarT _)) -> deriveDefault True cname
            AppT _ (ConT cname) -> deriveDefault False cname
            _ -> pure []


deriveCoercePass :: Name -> Q [Dec]
deriveCoercePass tyname = reify tyname >>= \case
    (TyConI (DataD [] _ [tyvarName -> p] _ cons _)) -> do
        p1 <- VarT <$> newName "p1"
        p2 <- VarT <$> newName "p2"
        let deps = nub $ concatMap findDependencies cons
        let cxt = map (makeCxt p1 p2) deps
        makeInstance p1 p2 cxt
            where 
                coercePass = ConT (mkName "CoercePass")
                
                findDependencies :: Con -> [Type]
                findDependencies (NormalC _ tys) = concatMap (findDepsInTy . snd) tys
                findDependencies (RecC _ tys) = concatMap (\(_,_,t) -> findDepsInTy t) tys
                findDependencies c = error $ "deriveCoercePass: invalid constructor type: " <> show c
                
                findDepsInTy :: Type -> [Type]
                findDepsInTy (AppT t (VarT p')) | p == p' = [t]
                findDepsInTy (ConT _) = []
                findDepsInTy ListT = []
                findDepsInTy (TupleT _) = []
                findDepsInTy (AppT t1 t2) = findDepsInTy t1 <> findDepsInTy t2
                findDepsInTy (ParensT t) = findDepsInTy t
                findDepsInTy t = error $ "deriveCoercePass: invalid type: " <> show t

                makeCxt :: Type -> Type -> Type -> Type
                makeCxt p1 p2 t = coercePass `AppT` (t `AppT` p1) `AppT` (t `AppT` p2)

                makeInstance :: Type -> Type -> Cxt -> Q [Dec]
                makeInstance p1 p2 cxt = pure $ [
                    InstanceD 
                        (Just Incoherent)
                        (cxt)
                        (coercePass `AppT` (ConT tyname `AppT` p1) `AppT` (ConT tyname `AppT` p2)) 
                        []
                    ]
    _ -> fail $ "deriveCoercePass: Invalid Type: " <> show tyname

tyvarName :: TyVarBndr a -> Name
tyvarName (PlainTV name _) = name
tyvarName (KindedTV name _ _) = name

typeContains :: Type -> Type -> Bool
typeContains x needle = go x
    where
        go t | t == x = True
        go (ConT _) = False
        go (VarT _) = False
        go ListT = False
        go (TupleT _) = False
        go (AppT t1 t2) = go t1 || go t2
        go (ParensT t) = go t
        go t = error $ "typeContains: invalid type: " <> show t


makeCompleteX :: Name -> [Name] -> Q [Dec]
makeCompleteX tyName pats = reify tyName >>= \case
    (TyConI (DataD [] _ _ _ (mapMaybe conName -> conNames) _)) -> pure <$> pragCompleteD (conNames <> pats) Nothing
    ty -> fail $ "makeCompleteX: non-standard data type: " <> show ty
    where
        conName = \case
            NormalC name _
                | last (nameBase name) == 'X' -> Nothing
                | otherwise                   -> Just name
            c -> error $ "makeCompleteX: non-standard constructor: " <> show c
