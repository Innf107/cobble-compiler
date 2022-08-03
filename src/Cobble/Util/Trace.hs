module Cobble.Util.Trace where

import Relude hiding (trace, traceM, traceId, traceShow, traceShowId, traceShowM, traceShowWith)

import Unsafe.Coerce

import Debug.Trace qualified as T

import GHC.Read

data TraceType = TraceQualify
               | TraceTC
               | TraceSolver
               | TraceUnify
               | TraceSubst
               | TraceLower
               | TraceCoreLint
               deriving (Show, Eq, Generic)

instance Read TraceType where
    readsPrec _ = \case
        "qualify"-> result TraceQualify
        "tc"     -> result TraceTC
        "solver" -> result TraceSolver
        "unify"  -> result TraceUnify
        "subst"  -> result TraceSubst
        "lower"  -> result TraceLower
        _ -> []
        where
            result tt = [(tt, "")]

class Trace where
    trace :: TraceType -> Text -> a -> a

traceId :: Trace => TraceType -> Text -> Text
traceId lvl a = trace lvl a a

traceShow :: (Trace, Show a) => TraceType -> a -> b -> b
traceShow lvl = trace lvl . show

traceShowId :: (Trace, Show a) => TraceType -> a -> a
traceShowId lvl a = trace lvl (show a) a

traceM :: (Trace, Applicative f) => TraceType -> Text -> f ()
traceM lvl msg = trace lvl msg $ pure ()

traceShowM :: (Trace, Applicative f) => TraceType -> Text -> f ()
traceShowM lvl = traceM lvl . show

traceShowWith :: (Trace, Show b) => TraceType -> (a -> b) -> a -> a
traceShowWith lvl f x = traceShow lvl (f x) x

data Dict c where
    Dict :: c => Dict c

data FakeDict c where
    FakeDict :: c -> FakeDict c

newtype TraceCarrier = TraceCarrier {
        _trace :: forall a. TraceType -> Text -> a -> a
    }

runTrace :: (forall a. TraceType -> Text -> a -> a) -> (Trace => b) -> b
runTrace traceImpl x = case unsafeCoerce (FakeDict (TraceCarrier traceImpl)) :: Dict Trace of
    Dict -> x

ignoreTrace :: (Trace => b) -> b
ignoreTrace = runTrace (\_ _ x -> x)

runTraceStdout :: (TraceType -> Bool) -> (Trace => b) -> b
runTraceStdout pred = runTrace impl
    where
        impl :: TraceType -> Text -> a -> a
        impl level msg x
            | pred level = T.trace (toString msg) x
            | otherwise  = x

runTraceStderrWith :: (TraceType -> Bool) -> (TraceType -> Text -> Text) -> (Trace => b) -> b
runTraceStderrWith pred pretty = runTrace impl
    where
        impl :: TraceType -> Text -> a -> a
        impl level msg x
            | pred level = T.trace (toString (pretty level msg)) x
            | otherwise  = x



