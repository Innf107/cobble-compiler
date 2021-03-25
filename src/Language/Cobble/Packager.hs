module Language.Cobble.Packager where

import Language.Cobble.Prelude
import Language.Cobble.MCAsm.Types
import Language.Cobble.Util.Polysemy.Time

import Codec.Archive.Zip

type PackageC (r :: EffectRow) = Members '[Time] r 

data DataPackOptions = DataPackOptions {
        name::Text
      , description::Text
    }

dataPackOptions :: Text -> Text -> DataPackOptions
dataPackOptions = DataPackOptions

makeDataPack :: forall r. (PackageC r) => DataPackOptions -> [CompiledModule] -> Sem r LByteString
makeDataPack options ms = fromArchive <$> do
    ia <- initialArchive
    foldlM (\a m -> addEntryToArchive <$> makeModEntry m <*> pure a) ia ms
    where
        makeModEntry :: CompiledModule -> Sem r Entry
        makeModEntry m = toEntry ("/data" </> toString (name options) </> "functions" </> show (compModName m) <> ".mcfunction")
                         <$> getTime 
                         <*> pure (encodeUtf8 (compModInstructions m))
                         
        initialArchive :: Sem r Archive
        initialArchive = do
            t <- getTime
            pure $ addEntryToArchive (toEntry "/pack.mcmeta" t (packMcMeta options)) emptyArchive
    -- TODO: Include data/minecraft/tags/functions/load.mcfunction, clean module and init module

packMcMeta :: DataPackOptions -> LByteString
packMcMeta options = mconcat [
      "{"
    , "    \"pack\":{"
    , "         \"pack_format\":6"
    , "         \"description\": \"" <> encodeUtf8 (description options) <> "\""
    , "    }"
    , "}"
    ]

