module FullScale.FileTest where

import Language.Cobble.Prelude

import qualified Data.Text as T

import Server.Framework hiding (TestError)
import qualified Server.Framework as F

import Data.Char

import System.Directory
import Language.Cobble (CompilationError(..))

data FileTest = FileTest {
        testName :: Text
    ,   testType :: TestType
    ,   testCode :: Text
    ,   testArgs :: [TestArg]
    } deriving (Show, Eq)

data TestType = TestExprScore Int
              | TestScore
              | TestError
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
parseTestType t = maybeToRight ("Invalid test type in: '" <> t <> "'") $ asumMap ($ t) [parseExprScore, parseScore, parseError]
        
parseExprScore :: Text -> Maybe TestType
parseExprScore t = TestExprScore <$> (T.stripPrefix "exprScore" (T.dropAround isSpace t) >>= readMaybe . toString)

parseScore :: Text -> Maybe TestType
parseScore t = if (T.dropAround isSpace t == "score") then Just TestScore else Nothing

parseError :: Text -> Maybe TestType
parseError t = if (T.dropAround isSpace t == "error") then Just TestError else Nothing

parseErrorType :: Text -> Maybe (CompilationError -> Bool, Text)
parseErrorType t = case T.split (==':') t of
    [errorTy, desc] -> (,desc) <$> case T.dropAround isSpace errorTy of
        "TypeError" -> Just \case {TypeError _ -> True; _ -> False}
        _ -> Nothing
    _ -> Nothing

fileTestToTest :: FileTest -> [Test]
fileTestToTest FileTest{testName, testType, testCode, testArgs} = case testType of
    TestExprScore score -> [testExprScore testName testCode score]
    TestScore -> (\f -> zipWith f testArgs [(1::Int)..]) \(TestArg{argHeader, argCode}) i -> case readMaybe (toString argHeader) of
        Just expected ->
            testSingleModScore
                (testName <> " [" <> show i <> "]")
                (testCode <> "main :: Unit; main = _setTestScoreboardUnsafe (" <> argCode <> ");") expected
        Nothing -> Error $ "Invalid integer for expected score: " <> argHeader
    TestError -> (\f -> zipWith f testArgs [(1::Int)..]) \(TestArg{argHeader, argCode}) i -> case parseErrorType argHeader of
        Just (predicate, desc) ->
            F.TestError
                (testName <> " [" <> show i <> "]")
                [TModule "test.cb" (testCode <> "\n\n__const__ :: a -> b -> b; __const__ x y = y;\n" <> "main :: Int; main = __const__ 42 (" <> argCode <> ");")]
                desc
                predicate
        Nothing -> Error $ "Invalid error type for expected error: " <> argHeader

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

