module Language.Cobble.Interactive where

import Language.Cobble.Prelude

import Language.Cobble.Interactive.Effect qualified as E

import Language.Cobble as C

runInteractive :: Sem (E.Interactive : r) a -> Sem r a
runInteractive = evalState initialInteractiveState . reinterpret \case
    E.Eval cmd -> eval cmd

data InteractiveState = InteractiveState {
        
    }

initialInteractiveState :: InteractiveState
initialInteractiveState = InteractiveState {}

eval :: Members '[State InteractiveState] r => Text -> Sem r Text
eval = do
    undefined
