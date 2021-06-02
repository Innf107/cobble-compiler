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
    } deriving (Show, Eq)

data TestType = TestExprScore Int
--            | TestScore  -- TODO
              deriving (Show, Eq)

splitTestFile :: Text -> Either Text [FileTest]
splitTestFile content = segments & traverse \x -> case T.split (==':') x of
    [] -> Left "missing test type"
    (testName:ts) ->  case T.split (=='\n') (T.intercalate ":" ts) of
        (typeStr:ts') -> parseTestType typeStr >>= \testType -> Right FileTest{testName=T.dropAround isSpace testName, testType, testCode=T.intercalate "\n" ts'}
        [] -> Left "Duplicate test string on line"
    where
        segments = filter (not . T.null) $ T.splitOn "--#" content
        
parseTestType :: Text -> Either Text TestType
parseTestType t = maybeToRight ("Invalid test type in: '" <> t <> "'") $ asumMap ($ t) [parseExprScore]
        
parseExprScore :: Text -> Maybe TestType
parseExprScore t = TestExprScore <$> (T.stripPrefix "exprScore" (T.dropAround isSpace t) >>= readMaybe . toString)


fileTestToTest :: FileTest -> Test
fileTestToTest FileTest{testName, testType, testCode} = case testType of
    TestExprScore score -> testExprScore testName testCode score

testFileContent :: Text -> [Test]
testFileContent content = case splitTestFile content of
    Left e -> [Error e]
    Right fts -> map fileTestToTest fts

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

