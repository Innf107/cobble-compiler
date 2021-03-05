{-#LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DataKinds, LambdaCase, BlockArguments #-}
module Server where

import Language.MCScript.Prelude
import Language.MCScript.Types
import Language.MCScript
import Lib

import qualified Data.Either as E

import qualified Data.Text as T

import System.Directory
import System.Process
import System.IO hiding (putStrLn, print)
import Control.Concurrent (threadDelay)

test :: IO ()
test = do
    testWithServer [] []

type Query = Text

testWithServer :: [Module 'Untyped] -> [(Text, Query, Expectation)] -> IO ()
testWithServer program tests = do
    cwd <- getCurrentDirectory <&> (</> "test/Server")
    
    removeDirectoryRecursive $ cwd </> "world"
    copyFileOrDirectory True (cwd </> "worldTEMPLATE") (cwd </> "world")
    
    mapFromLeft (fail . toString . (("Compilation failed: "::Text) <>) . show) $
        compileToFunctionsAtPath (cwd </> "world/datapacks/Test/data/") "Test" True program
    
    merrors <- runWithServer \sin sout -> forM tests \(desc, query, expectation) -> do
        whenM (hReady sout) $ waitUntil (hGetChar sout >> not <$> (hReady sout))
        hPutStrLn sout (toString query)
        success <- testExpectation sin sout desc query expectation
        case success of
            Nothing -> putTextLn $ desc <> ": SUCCESS!"
            Just received -> do
                putTextLn $ desc <> ": FAILED!!!\n"
                putTextLn $ "    expected: " <> showExpectation expectation
                putTextLn  $ "    received: '" <> received <> "'"
                putTextLn $ "    query:    '" <> query <> "'" 
        pure success
    case catMaybes merrors of
        [] -> putTextLn "\n~~~All tests passed successfully!~~~" 
        errors -> putTextLn $ "\n~~~" <> show (length errors) <> " tests FAILED" <> "~~~"


testExpectation :: Handle -> Handle -> Text -> Query -> Expectation -> IO (Maybe Text)
testExpectation sin sout desc query = \case
    ExpectLine line -> do
        hPutStrLn sin (toString query)
        res <- toText <$> hGetLine sout
        if res == line 
            then pure Nothing
            else pure $ Just res 
  
showExpectation :: Expectation -> Text
showExpectation = \case
    ExpectLine line -> "Single Line: '" <> line <> "'"
  
data Expectation = ExpectLine Text deriving (Show, Eq)

mapFromLeft :: (a -> b) -> Either a b -> b
mapFromLeft f (Left x) = f x
mapFromLeft _ (Right y) = y

runWithServer :: (Handle -> Handle -> IO a) -> IO a
runWithServer f = do
    cwd <- getCurrentDirectory <&> (</> "test/Server")
    (msin, msout, mserr, handle) <- createProcess
        ((proc "java" ["-jar", "server.jar", "-nogui"]) {std_in=CreatePipe, std_out=CreatePipe, cwd=Just cwd})
    case liftA2 (,) msin msout of
        Nothing -> terminateProcess handle >> fail "Invalid stdin or stdout stream"
        Just (sin, sout) -> do
            waitUntil $ T.isInfixOf "Done" . toText <$> hGetLine sout
            putStrLn "Server started"
            f sin sout <* interruptProcessGroupOf handle


waitUntil :: IO Bool -> IO ()
waitUntil m = m >>= bool (waitUntil m) pass

