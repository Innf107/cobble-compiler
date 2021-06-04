module FullScale.FileTest where

import Language.Cobble.Prelude

import qualified Data.Text as T

import Server.Framework

import Data.Char

import System.Directory

data FileTest = FileTest {
        testName :: Text
    ,   testType :: TestType
    ,   testCode :: Text
    ,   testArgs :: [TestArg]
    } deriving (Show, Eq)

data TestType = TestExprScore Int
              | TestScore
              deriving (Show, Eq)

data TestArg = TestArg {
    argHeader :: Text,
    argCode :: Text
} deriving (Show, Eq)

splitTestFile :: Text -> Either Text [FileTest]
splitTestFile content = segments & traverse \x -> case T.split (==':') x of
    [] -> Left "missing test type"
    (testName:ts) ->  case T.split (=='\n') (T.intercalate ":" ts) of
        (typeStr:ts') -> case T.splitOn "--!" (T.intercalate "\n" ts') of
            (code : args) -> parseTestType typeStr >>= \testType -> Right FileTest{testName=T.dropAround isSpace testName, testType, testCode=code, testArgs = map parseArg args}
            [] -> error "unreachable"
        [] -> Left "Duplicate test string on line"
    where
        segments = filter (not . T.null) $ T.splitOn "--#" content
        parseArg :: Text -> TestArg
        parseArg (T.break (=='\n') -> (header, code)) = TestArg header code

parseTestType :: Text -> Either Text TestType
parseTestType t = maybeToRight ("Invalid test type in: '" <> t <> "'") $ asumMap ($ t) [parseExprScore, parseScore]
        
parseExprScore :: Text -> Maybe TestType
parseExprScore t = TestExprScore <$> (T.stripPrefix "exprScore" (T.dropAround isSpace t) >>= readMaybe . toString)

parseScore :: Text -> Maybe TestType
parseScore t = if (T.dropAround isSpace t == "score") then Just TestScore else Nothing

fileTestToTest :: FileTest -> [Test]
fileTestToTest FileTest{testName, testType, testCode, testArgs} = case testType of
    TestExprScore score -> [testExprScore testName testCode score]
    TestScore -> (\f -> zipWith f testArgs [(1::Int)..]) \(TestArg{argHeader, argCode}) i -> case readMaybe (toString argHeader) of
        Just expected ->
            testSingleModScore
                (testName <> " [" <> show i <> "]")
                (testCode <> "main :: Unit; main = _setTestScoreboardUnsafe (" <> argCode <> ");") expected
        Nothing -> Error $ "Invalid integer for expected score: " <> argHeader

testFileContent :: Text -> [Test]
testFileContent content = case splitTestFile content of
    Left e -> [Error e]
    Right fts -> concatMap fileTestToTest fts

testFile :: FilePath -> IO [Test]
testFile fp = readFileText fp <&> (Header (toText fp) :) . testFileContent
       
runFileTests :: [FilePath] -> IO ()
runFileTests files = testWithServer . concat =<< traverse testFile files
       

runFileTestsRecursive :: FilePath -> IO ()
runFileTestsRecursive root = runFileTests =<< findTestFilesRecursive root

findTestFilesRecursive :: FilePath -> IO [FilePath] 
findTestFilesRecursive root = fmap concat . traverse (\p -> doesDirectoryExist (root </> p) >>= \case
        True | p `notElem` [".", ".."] -> findTestFilesRecursive (root </> p)
        False | ".cbt" `isExtensionOf` p -> pure [root </> p]
        _ -> pure []
    ) =<< getDirectoryContents root 

