{-#LANGUAGE TemplateHaskell#-}
module Cobble.Util.Polysemy.Context where

import Cobble.Prelude

data Context c m a where
    GetContext :: Context c m (Seq c)
    WithContext :: c -> m a -> Context c m a
    ModifyContext :: (Seq c -> Seq c) -> m a -> Context c m a
    
makeSem ''Context

runContext :: Sem (Context c : r) a -> Sem r a
runContext = runContextInitial []

runContextInitial :: Seq c -> Sem (Context c : r) a -> Sem r a
runContextInitial i = interpretH \case
    GetContext -> pureT i
    WithContext c m -> do
        m' <- runT m
        raise $ runContextInitial (c <| i) m'
    ModifyContext f m -> do
        m' <- runT m
        raise $ runContextInitial (f i) m'
