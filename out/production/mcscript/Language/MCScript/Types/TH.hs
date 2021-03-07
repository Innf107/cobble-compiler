{-#LANGUAGE NoImplicitPrelude, TemplateHaskell, BlockArguments, LambdaCase, OverloadedStrings#-}
{-#LANGUAGE PatternSynonyms#-}
module Language.MCScript.Types.TH where

import Language.MCScript.Prelude

import Language.Haskell.TH

import Data.Foldable (foldr1)

makeSynonyms :: Name -> Name -> Q [Dec]
makeSynonyms passVariant name = do
    TyConI (DataD _ _ (passT:_) _ constrs _) <- reify name
    forM constrs \case
        NormalC cname ps -> do
            let cnameU = mkName $ nameBase cname <> "U"
            pure $ PatSynSigD cnameU (foldr1 AppT (map (snd) ps))
            -- replace all occurances of 'passT' with 'passVariant'
            
            
