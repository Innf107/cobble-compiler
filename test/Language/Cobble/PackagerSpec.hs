module Language.Cobble.PackagerSpec where

import Language.Cobble.Prelude

import Language.Cobble.Types
import Language.Cobble.Packager
import Language.Cobble.MCAsm.Types

import Language.Cobble.Util.Polysemy.Time

import Codec.Archive.Zip

import Test.Hspec

spec :: Spec
spec = do
    pass
    {-
    describe "makeDataPack" $ beforeAll makeTestPack do
        it "includes the correct pack.mcmeta at path /pack.mcmeta" \(toArchive -> p) -> do
            fromEntry <$> (findEntryByPath "/pack.mcmeta" p)
                `shouldBe` Just (packMcMeta testPackOptions)
        it "includes all modules at the paths /data/<name>/functions/<modname>.mcfunction" \(toArchive -> p) -> do
            filesInArchive p `shouldContainAnyOrder` ["data/test/functions/test.mcfunction", "data/test/functions/mod1.test.mcfunction"]
    where
        makeTestPack :: IO LByteString
        makeTestPack = runM $ timeToIO $ makeDataPack testPackOptions [
                CompiledModule "test" "say Test\nsay More Tests"
            ,   CompiledModule "mod1.test" "say Module1.Test"
            ]
        testPackOptions :: DataPackOptions
        testPackOptions = DataPackOptions {
            name="test"
        ,   description="test description" 
        ,   target=target117
        }
-}
shouldContainAnyOrder :: (Eq a) => [a] -> [a] -> Expectation
shouldContainAnyOrder xs ys = all (\y -> y `elem` xs) ys `shouldBe` True
