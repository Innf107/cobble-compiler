module Network.RCON.Minecraft (
      Rcon
    , ServerInfo(..)
    , runRcon
    , sendCommand
    ) where

import Relude
import Language.Cobble.Util.Convert
import Network.Connection as C
import System.Random
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Serialize
import Control.Exception (bracket)
import System.Timeout
import Control.Concurrent (threadDelay)

newtype Rcon a = Rcon {unRcon :: ReaderT RConEnv IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

data RConEnv = RConEnv {
      rconConnection :: !Connection
    , rconResponseTimeout :: !Int
}

data ServerInfo = ServerInfo {
      serverHost :: !Text
    , serverPort :: !Int
    , serverPassword :: !Text
    }

runRcon :: ServerInfo -> Int -> Rcon a -> IO a
runRcon serverInfo respTimeout r = do
    bracket (do
            ctx <- initConnectionContext
            connectTo ctx $ ConnectionParams {
                connectionHostname  = toString $ serverHost serverInfo
                , connectionPort      = fromIntegral $ serverPort serverInfo
                , connectionUseSecure = Nothing
                , connectionUseSocks  = Nothing
                })
        (connectionClose)
        (\con -> runReaderT (unRcon (sendLogin (serverPassword serverInfo) *> r)) $ RConEnv con respTimeout)

sendCommand :: Text -> Rcon (Maybe Text)
sendCommand c = do
    pid <- liftIO $ randomIO @Int32
    let payload = encodeUtf8 c
    let len = fromIntegral $ B.length payload + 10
    sendPacket Packet {
      packetLength=len
    , packetId=pid
    , packetType=2
    , packetPayload=payload
    }
    fmap decodeUtf8 <$> receivePayload

-- Private definitions

data Packet = Packet {
    packetLength::Int32
  , packetId :: Int32
  , packetType::Int32
  , packetPayload::ByteString
}

askEnv :: Rcon RConEnv
askEnv = Rcon $ ask

asksEnv :: (RConEnv -> a) -> Rcon a
asksEnv = Rcon . asks

askConnection :: Rcon Connection
askConnection = asksEnv rconConnection


serializePacket :: Packet -> ByteString
serializePacket p = runPut (traverse_ putInt32le [packetLength p, packetId p, packetType p]) <> packetPayload p <> "\0\0"

sendPacket :: Packet -> Rcon ()
sendPacket p = do
    c <- askConnection
    liftIO $ connectionPut c (serializePacket p)

receivePayload :: Rcon (Maybe ByteString)
receivePayload =  do
    c <- askConnection
    respTimeout <- asksEnv rconResponseTimeout
    liftIO $ timeout respTimeout do
        len <- readInt32LE <$> connectionGetExact c 4
        rest <- connectionGetExact c (fromIntegral len)
        pure $ B.take (fromIntegral (len - 10)) $ B.drop 8 rest

readInt32LE :: ByteString -> Int32
readInt32LE bs = case runGet getInt32le bs of
    Right i -> i
    Left e -> error (toText e)


sendLogin :: Text -> Rcon ()
sendLogin pw = do
    pid <- liftIO $ randomIO @Int32
    let payload = encodeUtf8 pw
    let len = fromIntegral $ B.length payload + 10
    sendPacket Packet {
      packetLength=len
    , packetId=pid
    , packetType=3
    , packetPayload=payload
    }
    c <- askConnection
    liftIO $ do
        reslen <- readInt32LE <$> connectionGetExact c 4
        _ <- readInt32LE <$> connectionGetExact c 4
        t <- readInt32LE <$> connectionGetExact c 4
        _ <- connectionGetExact c (fromIntegral reslen - 8)
        if t == -1 then
            fail "Incorrect Password"
        else
            pass
