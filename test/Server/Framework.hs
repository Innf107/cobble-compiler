module Server.Framework where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble
import Language.Cobble.Util.Polysemy.Time
import Lib
import Network.RCON.Minecraft

import qualified Data.Either as E

import qualified Data.Text as T

import System.Directory
import System.Process
import System.IO hiding (putStrLn, print)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)


import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as DTime

type Description = Text

type Query = [Text]

data Test = Test Description [TModule] [TestQuery]
          | Header Text
          | Error Text
          deriving (Show, Eq)

data TModule = TModule FilePath Text deriving (Show, Eq)

data TestQuery = TestQuery Query Expectation deriving (Show, Eq)

test :: Description -> TModule -> [TestQuery] -> Test
test desc = Test desc . pure

testSingleMod' :: Description -> Text -> [Text] -> [TestQuery] -> Test
testSingleMod' desc src setup queries = Test desc [TModule "test.cb" src] (TestQuery (setup <> ["function test:test.main"]) DontExpect : queries)

testSingleMod :: Description -> Text -> [TestQuery] -> Test
testSingleMod desc src queries = testSingleMod' desc src [] queries

testSingleModScore :: Description -> Text -> Int -> Test
testSingleModScore desc src val = testSingleMod' desc src 
    ["scoreboard objectives remove test", "scoreboard objectives add test dummy"] 
    [TestQuery ["scoreboard players get test test"] $ ExpectLast $ expectScore "test" "test" val]

testExprScore :: Description -> Text -> Int -> Test
testExprScore desc src = testSingleModScore desc ("main :: Unit; main = _setTestScoreboardUnsafe (" <> src <> ");")

expectScore :: Text -> Text -> Int -> ExpectInner
expectScore score player val = ExpectExact (player <> " has " <> show val <> " [" <> score <> "]")

testWithServer :: [Test] -> IO ()
testWithServer categories = do
    cwd <- getCurrentDirectory <&> (</> "test/Server")

    logLn "Resetting 'world' to 'worldTEMPLATE'"
    removeDirectoryRecursive $ cwd </> "world"
    copyFileOrDirectory True (cwd </> "worldTEMPLATE") (cwd </> "world")

    logLn "Starting server for tests (Make sure *:25565 and *:25575 are unused)" 
    runWithServer
        $ runRcon (ServerInfo {serverHost="localhost", serverPort=25575, serverPassword="test"})
        $ for_ categories \case
        (Header t) -> liftIO $ headerLn t
        (Error e)  -> liftIO $ failLn e
        (Test desc program tests) -> do
            let opts = CompileOpts {
                name="test"
            ,   debug=True
            ,   target=target116
            ,   ddumpAsm=False
            ,   description="testing"
            }
            liftIO $ logLn "Compiling Cobble code"
            (logs, edatapack) <- liftIO $ runControllerC opts $ timeToIO $ compileContentsToDataPack (map (\(TModule x y) -> (x, y)) program)
            case edatapack of
                Left err -> liftIO do
                    timeText <- toText . DTime.formatTime DTime.defaultTimeLocale "\n[%d/%m/%Y %H:%M:%S]\n" <$> DTime.getCurrentTime
                    appendFileText "tests.log" (timeText <> show err <> "\nLOGS:" <> mconcat (map show logs))
                    failLn (desc <> ": COMPILATION FAILURE! " <> show err)
                Right dp -> do
                    liftIO $ writeFileLBS ("test/Server/world/datapacks/" <> "test.zip") dp >> logLn "Successfully compiled"
                    liftIO $ logLn "Running tests"
                    success <- getAll . mconcat <$> forM (zip [(1 :: Int)..] tests) \(i, (TestQuery query expectation)) -> do
                        sendCommand "reload"
                        sendCommand "test:clean"
                        sendCommand "test:init"
                        ress <- traverse sendCommand query
                        liftIO $ if (ress `matchesExpectation` expectation)
                            then pure $ All True
                            else failLn (desc <> "[" <> show i <> "]: FAILED!!!\n    Expected: " <> showExpectation expectation <> "\n    Got: " <> show ress) >> pure (All False)
                    when success $ liftIO $ successLn (desc <> ": passed")

matchesExpectation :: [Text] -> Expectation -> Bool
matchesExpectation res = \case
    ExpectAll i -> all (matchesExpectationInner i) res
    ExpectLast i -> fromMaybe False (matchesExpectationInner i <$> viaNonEmpty last res)
    ExpectList i -> all id $ zipWith (matchesExpectationInner) i res
    DontExpect -> True

matchesExpectationInner :: ExpectInner -> Text -> Bool
matchesExpectationInner ei r = case ei of
    ExpectExact t -> t == r

showExpectation :: Expectation -> Text
showExpectation = \case
    ExpectLast i -> "Last matching: " <> showExpectationInner i
    ExpectAll i -> "All matching: " <> showExpectationInner i
    ExpectList is -> "Matching pairwise: " <> show (map showExpectationInner is)
    DontExpect -> "Nothing in particular"

showExpectationInner :: ExpectInner -> Text
showExpectationInner = \case
    ExpectExact t -> "Exact match: '" <> t <> "'"


data Expectation = ExpectLast (ExpectInner)
                 | ExpectAll  (ExpectInner)
                 | ExpectList [ExpectInner]
                 | DontExpect
                 deriving (Show, Eq)

data ExpectInner = ExpectExact Text deriving (Show, Eq)

runWithServer :: IO a -> IO a
runWithServer a = do
    cwd <- getCurrentDirectory <&> (</> "test/Server")
    withCreateProcess ((proc "java" ["-jar", "server.jar", "-nogui"]) {std_out=CreatePipe, cwd=Just cwd})
        (\_ msout _ _ ->  case msout of
            Nothing -> fail "Invalid stdout stream"
            Just sout -> do
                logLn "Waiting for the server to be ready"
                waitUntil $ ("RCON running on" `T.isInfixOf`) . toText <$> hGetLine sout
                logLn "Server started"
                a)

headerLn :: Text -> IO ()
headerLn t = putTextLn ("\ESC[38;2;0;255;255m\STX" <> t <> "\n" <> T.replicate (T.length t) "—" <> "\ESC[0m\STX")

successLn :: Text -> IO ()
successLn t = putTextLn ("\ESC[38;2;0;255;0m\STX" <> t <> "\ESC[0m\STX")

failLn :: Text -> IO ()
failLn t = putTextLn ("\ESC[38;2;255;0;0m\STX" <> t <> "\ESC[0m\STX")

logLn :: Text -> IO ()
logLn t = hPutStrLn stderr ("\ESC[38;2;120;120;120m\STX" <> toString t <> "\ESC[0m\STX")


waitUntil :: IO Bool -> IO ()
waitUntil m = m >>= bool (waitUntil m) pass

whenFlag :: Monoid m => Text -> Bool -> [(String, String)] -> m -> m
whenFlag flag def env v = if T.toCaseFold (toText $ lookupDefault (show def) (toString ("TEST" <> flag)) (M.fromList env)) == T.toCaseFold "true" then v else mempty
