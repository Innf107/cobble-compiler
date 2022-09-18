{-# LANGUAGE TemplateHaskell #-}

module Cobble.Serialize (
    Serialize,
    serState,
    updateSerState,
    writeBytes,
    serializeToBS,
    Serializable (..),
    SerializeState (..),
) where

import Cobble.Prelude
import Cobble.Syntax

import Data.Binary qualified as B

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder

{- Interface layout

    +==========================Header==========================+====Size====+
    | Header size                                              |      8     |
    | Type count                                               |      8     |
    | offset of type 0                                         |      8     |
    | offset of type 1                                         |      8     |
    | ...                                                      |     ...    |
    | type 0                                                   |      ?     |
    | type 1                                                   |      ?     |
    | ...                                                      |     ...    |
    +--------------------------ModSig--------------------------+------------+
    | ...                                                      |            |
    +------------------------CoreModSig------------------------+------------+
    | ...                                                      |            |
    +----------------------------------------------------------+------------+
-}

data SerializeState = SerializeState
    { builder :: Builder.Builder
    , amountWritten :: Int
    }

data Serialize m a where
    SerState :: Serialize m SerializeState
    UpdateSerState :: (SerializeState -> SerializeState) -> Serialize m ()
    WriteBytes :: ByteString -> Serialize m ()

makeSem ''Serialize

data DeserializeState = DeserializeState
    {
    }

data Deserialize m a where
    DeserState :: Deserialize m DeserializeState
    UpdateDeserState :: (DeserializeState -> DeserializeState) -> Deserialize m ()
    ReadBytes :: Int -> Deserialize m ByteString
    DeserError :: Text -> Deserialize m a

class Serializable a where
    serialize :: Member Serialize r => a -> Sem r ()
    deserialize :: Member Deserialize r => Sem r a

serializeToBS :: Sem (Serialize : r) a -> Sem r (ByteString, a)
serializeToBS act = do
    (state, result) <-
        act
            & runState initialState . reinterpret \case
                SerState -> get
                UpdateSerState f -> modify f
                WriteBytes bytes -> do
                    modify
                        \state@SerializeState{builder, amountWritten} ->
                            state
                                { builder = builder <> Builder.byteString bytes
                                , amountWritten = amountWritten + ByteString.length bytes
                                }
    let body = builder state

    let header = undefined

    -- TODO: Build header
    pure (undefined, result)
  where
    initialState = SerializeState{amountWritten = 0, builder = mempty}

serialSize' :: Member Serialize r => Sem r a -> Sem r (Int, a)
serialSize' act = do
    SerializeState{amountWritten = initialAmountWritten} <- serState
    result <- act
    SerializeState{amountWritten = amountWrittenAfter} <- serState
    pure (amountWrittenAfter - initialAmountWritten, result)

serialSize :: Member Serialize r => Sem r () -> Sem r Int
serialSize = fmap fst . serialSize'

runDeserialize :: Sem (Deserialize : r) a -> Sem r a
runDeserialize = undefined

makeSem ''Deserialize