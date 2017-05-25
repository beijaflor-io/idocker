{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kernel
  where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception           (IOException (..),
                                              SomeException (..))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Control
import qualified Crypto.Hash.Algorithms      as Crypto
import qualified Crypto.MAC.HMAC             as Crypto
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Lens             as Aeson
import           Data.Aeson.QQ
import qualified Data.ByteString             as ByteString
import qualified Data.ByteString.Char8       as Char8
import qualified Data.ByteString.Lazy        as ByteStringL
import           Data.Convertible
import qualified Data.HashMap.Strict         as HashMap
import           Data.Monoid
import qualified Data.Text                   as Text
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import           GHC.Exts                    (fromList)
import           Language.Dockerfile         as Dockerfile
import           Prelude                     hiding (log)
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import           System.Process
import           System.ZMQ4.Monadic

import           Configuration               hiding (key)
import           Interpreter

type CommandChannel = TChan InterpreterCommand
data KernelEvent = KernelEvent
type EventsChannel = TChan KernelEvent

  -- :: (Show a, Sender t2, AsValue t1, Convertible t3 Char8.ByteString,
  --     Aeson.ToJSON t1, Aeson.ToJSON t, Aeson.ToJSON a) =>
data KernelSocket s t = KernelSocket { ksSocket :: Socket s t
                                     , ksName   :: Text.Text
                                     , ksInput  :: EventsChannel
                                     , ksOutput :: TChan Text.Text
                                     }

initialize :: [String] -> IO (Maybe Configuration)
initialize (connectionFile:_) = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Aeson.decode <$> (ByteStringL.readFile connectionFile)

ipythonMain :: [String] -> IO ()
ipythonMain args = do
  Just cnf@Configuration {..} <- initialize args
  print cnf
  runZMQ $ do
    let buildAddr port = "tcp://" <> Text.unpack ip <> ":" <> show port
        makeSocket stype sport = do
          s <- socket stype
          bind s $ buildAddr sport
          return s
    liftIO $ log "setup" "Building sockets..."
    stdinSocket <- makeSocket Router stdin_port
    controlSocket <- makeSocket Router control_port
    iopubSocket <- makeSocket Pub iopub_port
    shellSocket <- makeSocket Router shell_port
    hbSocket <- makeSocket Rep hb_port
    hbThread <-
      fork $ forever $ do
        h <- receive hbSocket
        liftIO $ log "heartbeat" "Received Heartbeat"
        send hbSocket [] h
    controlThread <-
      fork $ forever $ do
        h <- receive controlSocket
        liftIO $ log "control" (show h)
        send controlSocket [] h
    shellThread <-
      fork $ forever $ do
        msg@Message {..} <- receive' shellSocket
        case msgHeader ^. Aeson.key "msg_type" . Aeson._String of
          "execute_request" ->
            undefined -- executeRequest
              iopubSocket
              msg
              cnf
          "kernel_info_request" -> do
            liftIO $ log "shell" ("Kernel Info Request - " <> show msg)
            sendMessage shellSocket $
              SendMessage
              { sendMessage_level = "shell"
              , sendMessage_type = "kernel_info_reply"
              , sendMessage_parentIdentities = msgId
              , sendMessage_parentHeader = msgHeader
              , sendMessage_parentDelim = msgDelim
              , sendMessage_message =
                  [aesonQQ|
                        {
                        "protocol_version": "5.0",
                        "ipython_version": [1, 1, 0, ""],
                        "language_version": [0, 0, 1],
                        "language": "docker",
                        "implementation": "docker",
                        "implementation_version": "1.1",
                        "language_info": {
                            "name": "simple_kernel",
                            "version": "1.0",
                            'mimetype': "",
                            'file_extension': "",
                            'pygments_lexer': "",
                            'codemirror_mode': "dockerfile",
                            'nbconvert_exporter': ""
                        },
                        "banner": ""
                        }
                                                            |]
              , sendMessage_metadata = (Aeson.object [])
              }
          _ -> liftIO $ log "shell" "No parse"
    forever $ do
      h <- receive stdinSocket
      liftIO $ log "stdin" (show h)
    return ()

receive' s = do
    (uuid : delim : sig : header : parentHeader : metadata : content : blobs) <- receiveMulti s
    return Message { msgHeader = let Just h = Aeson.decodeStrict header in h
                   , msgId = uuid
                   , msgDelim = delim
                   , msgParentHeader = let Just h = Aeson.decodeStrict parentHeader in h
                   , msgMetadata = let Just h = Aeson.decodeStrict metadata in h
                   , msgContent = let Just h = Aeson.decodeStrict content in h
                   , msgBlobs = blobs
                   }
  where
    loop delim acc = do
        p <- receive s
        if p == delim
            then return acc
            else loop delim (acc ++ [p])

log l m = putStrLn ("[" <> l <> "] " <> m)

sign :: ByteString.ByteString -> [ByteString.ByteString] -> ByteString.ByteString
sign key list =
  Char8.pack $
  show $
  Crypto.hmacGetDigest $
  (Crypto.hmac key (ByteString.concat list) :: Crypto.HMAC Crypto.SHA256)

data SendMessage = SendMessage { sendMessage_key :: Text.Text
                               , sendMessage_level :: String
                               , sendMessage_parentIdentities :: Char8.ByteString
                               , sendMessage_parentHeader :: Aeson.Value
                               , sendMessage_parentDelim :: Char8.ByteString
                               , sendMessage_metadata :: Aeson.Value
                               , sendMessage_message :: Aeson.Value
                               , sendMessage_type :: Text.Text
                               }

sendMessage s SendMessage{..} = do
  liftIO $ do
    log sendMessage_level ("Sending message " <> show sendMessage_message)
    hFlush stdout
  now <- liftIO getCurrentTime
  uuid <- liftIO $ toString <$> nextRandom
  let header =
        [aesonQQ|
                            {
                            "date": #{now},
                            "msg_id": #{uuid},
                            "username": "kernel",
                            "session": #{sendMessage_parentHeader ^. Aeson.key "session" . Aeson._String},
                            "msg_type": #{sendMessage_type :: Text.Text},
                            "version": "3.0"
                            }
                            |]
      signature =
        sign
          (convert sendMessage_key)
          [ ByteStringL.toStrict $ Aeson.encode header
          , ByteStringL.toStrict $ Aeson.encode sendMessage_parentHeader
          , ByteStringL.toStrict $ Aeson.encode sendMessage_metadata
          , ByteStringL.toStrict $ Aeson.encode sendMessage_message
          ]
  let parts =
        [ sendMessage_parentIdentities
        , sendMessage_parentDelim
        , signature
        , ByteStringL.toStrict $ Aeson.encode header
        , ByteStringL.toStrict $ Aeson.encode sendMessage_parentHeader
        , ByteStringL.toStrict $ Aeson.encode sendMessage_metadata
        , ByteStringL.toStrict $ Aeson.encode sendMessage_message
        ]
  liftIO $ do
    log sendMessage_level ("Sending parts " <> show parts)
    hFlush stdout
  sendMulti s $ fromList parts

data Message =
    Message { msgHeader       :: Aeson.Value
            , msgDelim        :: ByteString.ByteString
            , msgId           :: ByteString.ByteString
            , msgParentHeader :: Aeson.Value
            , msgMetadata     :: Aeson.Value
            , msgContent      :: Aeson.Value
            , msgBlobs        :: [ByteString.ByteString]
            }
  deriving(Show, Eq)

executeRequest iopubSocket msg@Message {..} cnf@Configuration {..} = do
  liftIO $ print msgContent
  now <- liftIO getCurrentTime
  let instructions =
        Dockerfile.parseString
          (convert (msgContent ^. Aeson.key "code" . Aeson._String))
  undefined -- sendMessage
    -- key
    iopubSocket
    "iopub"
    "status"
    msgId
    msgHeader
    msgDelim
    [aesonQQ|
{
  "execution_state": "busy"
}
|]
    (Aeson.object [])
  undefined -- sendMessage
    -- key
    iopubSocket
    "iopub"
    "execute_input"
    msgId
    msgHeader
    msgDelim
    [aesonQQ|
                                                                                          {
                                                                                            "execution_count": 1,
                                                                                            "code": #{msgContent ^. Aeson.key "code" . Aeson._String}
                                                                                          }
                                                                                          |]
    (Aeson.object [])
  undefined -- sendMessage
    -- key
    iopubSocket
    "iopub"
    "stream"
    msgId
    msgHeader
    msgDelim
    [aesonQQ|
                                                                                          {
                                                                                            "name": "stdout",
                                                                                            "text": #{show instructions}
                                                                                          }
                                                                                          |]
    (Aeson.object [])
  undefined -- sendMessage
    -- key
    iopubSocket
    "iopub"
    "execute_result"
    msgId
    msgHeader
    msgDelim
    [aesonQQ|
                                                                                          {
                                                                                            "execution_count": 1,
                                                                                            "data": {"text/html": "<code>ok</code>"},
                                                                                            "metadata": {}
                                                                                          }
                                                                                          |]
    (Aeson.object [])
  sendMessage
    iopubSocket
    SendMessage
    { sendMessage_level = "iopub"
    , sendMessage_type = "status"
    , sendMessage_parentIdentities = msgId
    , sendMessage_parentHeader = msgHeader
    , sendMessage_parentDelim = msgDelim
    , sendMessage_message =
        [aesonQQ|
                {
                "execution_state": "idle"
                }
                |]
    , sendMessage_metadata = (Aeson.object [])
    }
  sendMessage
    iopubSocket
    SendMessage
    { sendMessage_level = "shell"
    , sendMessage_type = "kernel_info_reply"
    , sendMessage_parentIdentities = msgId
    , sendMessage_parentHeader = msgHeader
    , sendMessage_parentDelim = msgDelim
    , sendMessage_message =
        [aesonQQ|
                {
                "protocol_version": "5.0",
                        "ipython_version": [1, 1, 0, ""],
                        "language_version": [0, 0, 1],
                        "language": "docker",
                        "implementation": "docker",
                        "implementation_version": "1.1",
                        "language_info": {
                            "name": "simple_kernel",
                            "version": "1.0",
                            'mimetype': "",
                            'file_extension': "",
                            'pygments_lexer': "",
                            'codemirror_mode': "dockerfile",
                            'nbconvert_exporter': ""
                        },
                        "banner": ""
                        }
                        |]
    , sendMessage_metadata = (Aeson.object [])
    }
