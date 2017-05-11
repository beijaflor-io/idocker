{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Monad.Trans.Control
import System.Process
import           Control.Concurrent.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Crypto.Hash.Algorithms    as Crypto
import qualified Crypto.MAC.HMAC           as Crypto
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Lens
import           Data.Aeson.QQ
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Char8     as Char8
import qualified Data.ByteString.Lazy      as ByteStringL
import           Data.Convertible
import qualified Data.HashMap.Strict       as HashMap
import           Data.Monoid
import qualified Data.Text                 as Text
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import           GHC.Exts                  (fromList)
import           Language.Dockerfile       as Dockerfile
import           System.Environment
import           System.IO
import           System.ZMQ4.Monadic

import           Configuration

initialize :: IO (Maybe Configuration)
initialize = do
    [connectionFile] <- getArgs
    Aeson.decode <$> (ByteStringL.readFile connectionFile)

data ExecutionState =
    ExecutionState

replMain :: IO ()
replMain = do
    _ <- runStateT runRepl Nothing
    return ()

type ReplState = Maybe (Handle, Handle, Handle, ProcessHandle)

runRepl :: (MonadBaseControl IO m, MonadState ReplState m, MonadIO m) => m ()
runRepl = do
  ln <-
    liftIO $ do
      putStr "> "
      hFlush stdout
      getLine
  let einstructions = Dockerfile.parseString ln
  case einstructions of
    Left err -> liftIO $ print err
    Right instructions ->
      forM_ instructions $ \(Dockerfile.InstructionPos instruction _ _) -> do
        liftIO $ print instruction
        executeInstruction instruction
  runRepl
  where
    executeInstruction :: (MonadBaseControl IO m, MonadState ReplState m, MonadIO m) => Instruction -> m ()
    executeInstruction i =
      case i of
        From (UntaggedImage img) -> do
          state <- get
          case state of
            Nothing -> do
              (Just hin, Just hout, Just herr, ph) <-
                liftIO $
                createProcess
                  (proc "docker" ["run", "-it", img])
                  { std_out = CreatePipe
                  , std_err = CreatePipe
                  , std_in = CreatePipe
                  }
              fork $ forever $ liftIO $ do
                  errLine <- hGetLine herr
                  hPutStrLn stderr errLine
              fork $ forever $ liftIO $ do
                  outLine <- hGetLine hout
                  putStrLn outLine
              put (Just (hin, hout, herr, ph))
            Just (_, _, _, ph) -> error "Process running"
        Run args -> undefined

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Just cnf@Configuration {..} <- initialize
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

    fork $
      forever $ do
        h <- receive hbSocket
        liftIO $ log "heartbeat" "Received Heartbeat"
        send hbSocket [] h
    fork $
      forever $ do
        h <- receive controlSocket
        liftIO $ log "control" (show h)
        send controlSocket [] h
    fork $
      forever $ do
        msg@Message{..} <- receive' shellSocket
        case msgHeader ^. Data.Aeson.Lens.key "msg_type" . _String of
          "execute_request" -> do
            liftIO $ print msgContent
            now <- liftIO getCurrentTime
            let instructions = Dockerfile.parseString (convert (msgContent ^. Data.Aeson.Lens.key "code" . _String))
            sendMessage key iopubSocket "iopub" "status" msgId msgHeader msgDelim [aesonQQ|
                                                                                          {
                                                                                            "execution_state": "busy"
                                                                                          }
                                                                                          |] (Aeson.object [])
            sendMessage key iopubSocket "iopub" "execute_input" msgId msgHeader msgDelim [aesonQQ|
                                                                                          {
                                                                                            "execution_count": 1,
                                                                                            "code": #{msgContent ^. Data.Aeson.Lens.key "code" . _String}
                                                                                          }
                                                                                          |] (Aeson.object [])
            sendMessage key iopubSocket "iopub" "stream" msgId msgHeader msgDelim [aesonQQ|
                                                                                          {
                                                                                            "name": "stdout",
                                                                                            "text": #{show instructions}
                                                                                          }
                                                                                          |] (Aeson.object [])
            sendMessage key iopubSocket "iopub" "execute_result" msgId msgHeader msgDelim [aesonQQ|
                                                                                          {
                                                                                            "execution_count": 1,
                                                                                            "data": {"text/html": "<code>ok</code>"},
                                                                                            "metadata": {}
                                                                                          }
                                                                                          |] (Aeson.object [])
            sendMessage key iopubSocket "iopub" "status" msgId msgHeader msgDelim [aesonQQ|
                                                                                          {
                                                                                            "execution_state": "idle"
                                                                                          }
                                                                                          |] (Aeson.object [])
            sendMessage key iopubSocket "iopub" "execute_reply" msgId msgHeader msgDelim [aesonQQ|
                                                                                                 {
                                                                                                   "status": "ok",
                                                                                                   "execution_count": 1,
                                                                                                   "user_variables": {},
                                                                                                   "payload": [],
                                                                                                   "user_expressions": {}
                                                                                                 }
                                                                                                 |]
                                                                                  [aesonQQ|
                                                                                          {
                                                                                            "dependencies_met": true,
                                                                                            "status": "ok",
                                                                                            "started": #{now}
                                                                                          }
                                                                                          |]
          "kernel_info_request" -> do
            liftIO $ log "shell" ("Kernel Info Request - " <> show msg)
            sendMessage key shellSocket "shell" "kernel_info_reply" msgId msgHeader msgDelim [aesonQQ|
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
                                    |] (Aeson.object [])
          _ -> liftIO $ log "shell" "No parse"
    forever $ do
      h <- receive stdinSocket
      liftIO $ log "stdin" (show h)
    return ()
  where
    sendMessage key s l msgType parentIdentities parentHeader parentDelim msg metadata = do
        liftIO $ do
            log l ("Sending message " <> show msg)
            hFlush stdout
        now <- liftIO getCurrentTime
        uuid <- liftIO $ toString <$> nextRandom
        let header = [aesonQQ|
                             {
                               "date": #{now},
                               "msg_id": #{uuid},
                               "username": "kernel",
                               "session": #{parentHeader ^. Data.Aeson.Lens.key "session" . _String},
                               "msg_type": #{msgType :: Text.Text},
                               "version": "3.0"
                             }
                             |]
            signature = sign (convert key) [ ByteStringL.toStrict $ Aeson.encode header
                                           , ByteStringL.toStrict $ Aeson.encode parentHeader
                                           , ByteStringL.toStrict $ Aeson.encode metadata
                                           , ByteStringL.toStrict $ Aeson.encode msg
                                           ]
        let parts = [ parentIdentities
                    , parentDelim
                    , signature
                    , ByteStringL.toStrict $ Aeson.encode header
                    , ByteStringL.toStrict $ Aeson.encode parentHeader
                    , ByteStringL.toStrict $ Aeson.encode metadata
                    , ByteStringL.toStrict $ Aeson.encode msg
                    ]
        liftIO $ do
            log l ("Sending parts " <> show parts)
            hFlush stdout
        sendMulti s $ fromList parts
    log l m = putStrLn ("[" <> l <> "] " <> m)
    sign :: ByteString.ByteString -> [ByteString.ByteString] -> ByteString.ByteString
    sign key list = Char8.pack $ show $ Crypto.hmacGetDigest $ (Crypto.hmac key (ByteString.concat list) :: Crypto.HMAC Crypto.SHA256)
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
