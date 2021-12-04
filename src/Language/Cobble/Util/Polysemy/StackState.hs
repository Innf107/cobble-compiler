{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Util.Polysemy.StackState where

import Language.Cobble.Prelude
import Data.Default

data StackState s m a where
    Sget    :: StackState s m s
    Sput    :: s -> StackState s m ()
    Smodify :: (s -> s) -> StackState s m ()
    Spush   :: s -> StackState s m ()
    Spop     :: StackState s m s

makeSem ''StackState

headDef :: Default a => [a] -> a
headDef (x : _) = x
headDef [] = def

_headDef :: Default a => Lens' [a] a
_headDef = lens lget lset
    where
        lget = headDef
        lset :: [a] -> a -> [a]
        lset [] x     = [x]
        lset (_:xs) y = (y : xs)

evalStackStateDef :: forall s r a. Default s => Sem (StackState s : r) a -> Sem r a
evalStackStateDef = evalState ([] :: [s]) . reinterpret \case
    Sget -> gets headDef
    Sput x -> modify (over _headDef (const x)) 
    Smodify f -> modify (over _headDef f)
    Spush s -> modify (s:)
    Spop -> state \case
        (s:ss) -> (s, ss)
        [] -> (def, [])

evalStackStatePanic :: forall s r a. HasCallStack => s -> Sem (StackState s : r) a -> Sem r a
evalStackStatePanic initial = evalState (initial :| []) . reinterpret \case
    Sget -> gets head
    Sput x -> modify (over head1 (const x))
    Smodify f -> modify (over head1 f)
    Spush s -> modify (s |:)
    Spop -> state \case
        (s :| (s':ss)) -> (s, (s' :| ss))
        _ -> error "evalStackStatePanic: empty state on spop" 

