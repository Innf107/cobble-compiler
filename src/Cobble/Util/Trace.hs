module Cobble.Util.Trace where

import Relude hiding (trace, traceM, traceId, traceShow, traceShowId, traceShowM, traceShowWith)

import Unsafe.Coerce

import Debug.Trace qualified as T

data TraceLevel = Warning     
                | Info        
                | Verbose     
                | Debug        
                | DebugVerbose
                | DebugVeryVerbose
                deriving (Show, Eq, Ord, Enum, Read)

class Trace where
    trace :: TraceLevel -> Text -> a -> a

traceId :: Trace => TraceLevel -> Text -> Text
traceId lvl a = trace lvl a a

traceShow :: (Trace, Show a) => TraceLevel -> a -> b -> b
traceShow lvl = trace lvl . show

traceShowId :: (Trace, Show a) => TraceLevel -> a -> a
traceShowId lvl a = trace lvl (show a) a

traceM :: (Trace, Applicative f) => TraceLevel -> Text -> f ()
traceM lvl msg = trace lvl msg $ pure ()

traceShowM :: (Trace, Applicative f) => TraceLevel -> Text -> f ()
traceShowM lvl = traceM lvl . show

traceShowWith :: (Trace, Show b) => TraceLevel -> (a -> b) -> a -> a
traceShowWith lvl f x = traceShow lvl (f x) x

data Dict c where
    Dict :: c => Dict c

data FakeDict c where
    FakeDict :: c -> FakeDict c

newtype TraceCarrier = TraceCarrier {
        _trace :: forall a. TraceLevel -> Text -> a -> a
    }

runTrace :: (forall a. TraceLevel -> Text -> a -> a) -> (Trace => b) -> b
runTrace traceImpl x = case unsafeCoerce (FakeDict (TraceCarrier traceImpl)) :: Dict Trace of
    Dict -> x

ignoreTrace :: (Trace => b) -> b
ignoreTrace = runTrace (\_ _ x -> x)

runTraceStdout :: TraceLevel -> (Trace => b) -> b
runTraceStdout maxLevel = runTrace impl
    where
        impl :: TraceLevel -> Text -> a -> a
        impl level msg x
            | level <= maxLevel = T.trace (toString msg) x
            | otherwise         = x

runTraceStderrWith :: TraceLevel -> (TraceLevel -> Text -> Text) -> (Trace => b) -> b
runTraceStderrWith maxLevel pretty = runTrace impl
    where
        impl :: TraceLevel -> Text -> a -> a
        impl level msg x
            | level <= maxLevel = T.trace (toString (pretty level msg)) x
            | otherwise         = x



