module Server where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble
import Lib
import Network.RCON.Minecraft

import qualified Data.Either as E

import qualified Data.Text as T

import System.Directory
import System.Process
import System.IO hiding (putStrLn, print)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)

test :: IO ()
test = do
    testWithServer [
            Module @'Typecheck mempty "test" [
                SetScoreboard () dli testScore "TestPlayer" (IntLit () dli 5)
            ]
        ] [("A basic SetScoreboard works",
            ["scoreboard objectives add TestScore dummy", "function test:test", "scoreboard players get TestPlayer TestScore"],
            ExpectLast (ExpectExact "TestPlayer has 5 [TestScore]"))] -- TODO: Probably fails, because TestScore was not created

dli :: LexInfo
dli = LexInfo 0 0 "Test"

testScore :: Objective
testScore = Objective "TestScore"

type Description = Text

type Query = [Text]

testWithServer :: a -> [(Description, Query, Expectation)] -> IO ()
testWithServer program tests = undefined {- do
    cwd <- getCurrentDirectory <&> (</> "test/Server")

    logLn "Resetting 'world' to 'worldTEMPLATE'"
    removeDirectoryRecursive $ cwd </> "world"
    copyFileOrDirectory True (cwd </> "worldTEMPLATE") (cwd </> "world")

    logLn "Compiling Cobble code"
    mapFromLeft (fail . toString . (("Compilation failed: "::Text) <>) . show) $
        compileToFunctionsAtPath' (cwd </> "world/datapacks/Test/data/") "test" True program

    let sInfo = ServerInfo {serverHost="localhost", serverPort=25575, serverPassword="test"}

    logLn "Starting server"
    runWithServer $ runRcon sInfo $ forM_ tests \(desc, query, expectation) -> do
        ress <- traverse sendCommand query
        liftIO $ if (ress `matchesExpectation` expectation)
            then putTextLn $ desc <> " passed"
            else putTextLn $ desc <> " FAILED!!!\n    Expected: " <> showExpectation expectation <> "\n    Got: " <> show ress
    logLn "Server tests finished"
-}

matchesExpectation :: [Text] -> Expectation -> Bool
matchesExpectation res = \case
    ExpectAll i -> all (matchesExpectationInner i) res
    ExpectLast i -> fromMaybe False (matchesExpectationInner i <$> viaNonEmpty last res)
    ExpectList i -> all id $ zipWith (matchesExpectationInner) i res

matchesExpectationInner :: ExpectInner -> Text -> Bool
matchesExpectationInner ei r = case ei of
    ExpectExact t -> t == r

showExpectation :: Expectation -> Text
showExpectation = \case
    ExpectLast i -> "Last matching: " <> showExpectationInner i
    ExpectAll i -> "All matching: " <> showExpectationInner i
    ExpectList is -> "Matching pairwise: " <> show (map showExpectationInner is)

showExpectationInner :: ExpectInner -> Text
showExpectationInner = \case
    ExpectExact t -> "Exact match: '" <> t <> "'"

  
data Expectation = ExpectLast (ExpectInner)
                 | ExpectAll  (ExpectInner)
                 | ExpectList [ExpectInner]
                 deriving (Show, Eq)

data ExpectInner = ExpectExact Text deriving (Show, Eq)

runWithServer :: IO a -> IO a
runWithServer a = do
    cwd <- getCurrentDirectory <&> (</> "test/Server")
    bracket
        (createProcess ((proc "java" ["-jar", "server.jar", "-nogui"]) {std_out=CreatePipe, cwd=Just cwd}))
        (\(_, _, _, handle) -> interruptProcessGroupOf handle)
        (\(_, msout, _, _) ->   case msout of
            Nothing -> fail "Invalid stdout stream"
            Just sout -> do
                logLn "Waiting for the server to be ready"
                waitUntil $ T.isInfixOf "RCON running on" . toText <$> hGetLine sout
                logLn "Server started"
                a)

logLn :: Text -> IO ()
logLn = putTextLn


waitUntil :: IO Bool -> IO ()
waitUntil m = m >>= bool (waitUntil m) pass


