{-#LANGUAGE TemplateHaskell #-}
module Language.Cobble.Types.TH where

import Language.Cobble.Prelude

import Language.Haskell.TH

import Data.Foldable (foldr1)

import qualified Data.List as L (tail)


makeSynonyms :: Name -> Name -> String -> Q [Dec]
makeSynonyms passVariantName name postfix = do
    TyConI (DataD _ _ (passT:_) _ constrs _) <- reify name
    concat <$> forM (filter (\(NormalC n _) -> nameBase n /= nameBase name <> "X") constrs) \case
        NormalC cname ps -> do
            let cnameU = mkName $ nameBase cname <> postfix
            passVariant <- typeOfVariant passVariantName
            varNames <- replicateM (length ps - 1) (newName "x")
            pure $ [
                  PatSynSigD cnameU (foldr1 (\x y -> AppT (AppT ArrowT x) y) (map (replaceTVar passT passVariant . snd) 
                    (L.tail ps ++ [(undefined, AppT (ConT name) passVariant)])))
                , PatSynD cnameU
                    (PrefixPatSyn varNames)
                    (ExplBidir [Clause (map VarP varNames)
                        (NormalB $ foldl' AppE (ConE cname) (ConE '() : map VarE varNames)) []])
                    (ConP cname (WildP : map VarP varNames))
                ]


typeOfVariant :: Name -> Q Type
typeOfVariant name = reify name <&> \case
    (DataConI n _ _) -> ConT n


replaceTVar :: TyVarBndr -> Type -> Type -> Type
replaceTVar needle repl = \case
    VarT n | n == tvarName needle -> repl
    ForallT bndr cntxt ty -> ForallT bndr cntxt (replaceTVar needle repl ty)
    ForallVisT bndr ty -> ForallVisT bndr (replaceTVar needle repl ty)
    AppT a b -> AppT (replaceTVar needle repl a) (replaceTVar needle repl b)
    AppKindT t k -> AppKindT (replaceTVar needle repl t) k
    SigT t k -> SigT (replaceTVar needle repl t) k
    ConT n -> ConT n
    PromotedT n -> PromotedT n
    InfixT x n y -> InfixT (replaceTVar needle repl x) n (replaceTVar needle repl y)
    UInfixT x n y -> UInfixT (replaceTVar needle repl x) n (replaceTVar needle repl y)
    ParensT t -> ParensT (replaceTVar needle repl t)
    TupleT i -> TupleT i
    UnboxedTupleT i -> UnboxedTupleT i
    UnboxedSumT i -> UnboxedSumT i
    ArrowT -> ArrowT
    EqualityT -> EqualityT
    ImplicitParamT s t -> ImplicitParamT s (replaceTVar needle repl t)
    x -> x

tvarName :: TyVarBndr -> Name
tvarName = \case
    PlainTV n -> n
    KindedTV n _ -> n
