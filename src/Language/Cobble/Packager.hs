module Language.Cobble.Packager where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.MCAsm.Types
import Language.Cobble.McFunction.Types
import Language.Cobble.McFunction.PrettyPrint
import Language.Cobble.Util.Polysemy.Time

import Codec.Archive.Zip

type PackageC (r :: EffectRow) = Members '[Time] r 

data DataPackOptions = DataPackOptions {
        name::Text
      , description::Text
    }

makeDataPack :: (PackageC r) => DataPackOptions -> [CompiledModule] -> Sem r LByteString
makeDataPack ops mods = makeDataPack' ops mods <$> getTime 

makeDataPack' :: DataPackOptions -> [CompiledModule] -> Integer -> LByteString
makeDataPack' options ms time = fromArchive $
    let namespacedMods = setNamespace (name options) ms in
        foldr (addEntryToArchive . makeModEntry) initialArchive (runtimeMods <> namespacedMods)
    where
        makeModEntry :: CompiledModule -> Entry
        makeModEntry (path, cmds) = toEntry ("/data" </> toString (name options) </> "functions" </> path <.> ".mcfunction")
                                    time 
                                    (encodeUtf8 (unlines (map prettyPrint cmds)))
                        
        initialArchive :: Archive
        initialArchive = addEntryToArchive (toEntry "/pack.mcmeta" time (packMcMeta options)) emptyArchive


packMcMeta :: DataPackOptions -> LByteString
packMcMeta options = mconcat $ map (<> "\n") [
      "{"
    , "    \"pack\":{"
    , "         \"pack_format\": 7,"
    , "         \"description\": \"" <> encodeUtf8 (description options) <> "\""
    , "    }"
    , "}"
    ]


setNamespace :: Text -> [CompiledModule] -> [CompiledModule]
setNamespace ns = transformBi \case
    (Own n) -> Foreign ns n
    x -> x

runtimeMods :: [CompiledModule]
runtimeMods = [
        ("init", [
            Gamerule "maxCommandChainLength" (GInt $ fromIntegral (maxBound :: Int32))
        ,   Forceload $ FAdd 0 0 
        ,   Scoreboard $ Objectives $ OAdd "REGS" "dummy" Nothing
        ,   Scoreboard $ Objectives $ OAdd "IX" "dummy" Nothing 
        ,   Scoreboard $ Objectives $ OAdd "APTR" "dummy" Nothing 
        ]),
        ("clean", [
            Scoreboard $ Objectives $ ORemove "REGS"
        ,   Scoreboard $ Objectives $ ORemove "IX"
        ,   Scoreboard $ Objectives $ ORemove "APTR"
        ])
    ]
