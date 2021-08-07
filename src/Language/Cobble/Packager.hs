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
      , target::Target
    }

makeDataPack :: forall r. (PackageC r) => DataPackOptions -> [CompiledModule] -> Sem r LByteString
makeDataPack options ms = fromArchive <$> do
    ia <- initialArchive
    foldlM (\a m -> addEntryToArchive <$> makeModEntry m <*> pure a) ia ms
    where
        makeModEntry :: CompiledModule -> Sem r Entry
        makeModEntry (path, cmds) = toEntry ("/data" </> toString (name options) </> "functions" </> path <.> ".mcfunction")
                         <$> getTime 
                         <*> pure (encodeUtf8 (unlines (map prettyPrint cmds)))
                         
        initialArchive :: Sem r Archive
        initialArchive = do
            t <- getTime
            pure $ addEntryToArchive (toEntry "/pack.mcmeta" t (packMcMeta options)) emptyArchive


packMcMeta :: DataPackOptions -> LByteString
packMcMeta options = mconcat $ map (<> "\n") [
      "{"
    , "    \"pack\":{"
    , "         \"pack_format\":" <> show (packFormat $ target $ options) <> ","
    , "         \"description\": \"" <> encodeUtf8 (description options) <> "\""
    , "    }"
    , "}"
    ]

