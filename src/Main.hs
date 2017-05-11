{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Monad.Trans.Control
import Control.Exception (IOException(..), SomeException(..))
import Control.Monad.Except
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

initialize :: [String] -> IO (Maybe Configuration)
initialize (connectionFile:_) = do
    Aeson.decode <$> (ByteStringL.readFile connectionFile)

data ExecutionState =
    ExecutionState

replMain :: IO ()
replMain = do
    _ <- runStateT runRepl (ReplState Nothing [] 0)
    return ()

data ReplState = ReplState { containerId :: Maybe String
                           , replDockerfile :: Dockerfile
                           , replInstructionCount :: Int
                           }
type ReplM m = (MonadError IOException m, MonadBaseControl IO m, MonadState ReplState m, MonadIO m)

data ReplCommand = ReplCommandDockerfile
                 | ReplCommandExit
                 | ReplCommandFlush
                 | ReplCommandEOL
                 | ReplCommandInstruction InstructionPos

runRepl :: ReplM m => m ()
runRepl = do
  run `catchError` (\e -> liftIO (print e))
  modify (\s -> s { replInstructionCount = replInstructionCount s + 1 })
  runRepl
  where
    run = readCommand >>= executeCommand
    readCommand :: ReplM m => m ReplCommand
    readCommand = do
      iCount <- replInstructionCount <$> get
      ln <-
        liftIO $ do
          putStrLn ""
          putStr ("  [" <> show iCount <> "] ")
          hFlush stdout
          ln <- getLine
          putStrLn ""
          return ln
      let einstructions = Dockerfile.parseString (ln)
      case einstructions of
        Left err -> do
            liftIO $ print err
            case ln of
              "DOCKERFILE" -> return ReplCommandDockerfile
              "FLUSH" -> return ReplCommandFlush
              "EXIT" -> return ReplCommandExit
              _ -> return (ReplCommandInstruction (InstructionPos (Run (words ln)) "Dockerfile" 0))
        Right (i:_) -> do
          return (ReplCommandInstruction i)
        _ -> return ReplCommandEOL
    executeCommand :: ReplM m => ReplCommand -> m ()
    executeCommand (ReplCommandInstruction i) = do
      modify (\s -> s { replDockerfile = replDockerfile s ++ [i] })
      executeInstruction i
    executeCommand ReplCommandEOL = return ()
    executeCommand ReplCommandExit = do
      mcontainerId <- containerId <$> get
      case mcontainerId of
          Just i ->
              liftIO $ do
                  putStrLn "Exiting..."
                  callProcess "docker" ["stop", i]
                  callProcess "docker" ["rm", i]
          _ -> return ()
    executeCommand ReplCommandFlush = do
      state <- replDockerfile <$> get
      let df = prettyPrint state
      liftIO $ writeFile "./Dockerfile" df
    executeCommand ReplCommandDockerfile = do
      state <- replDockerfile <$> get
      liftIO $ putStrLn (prettyPrint state)
    executeInstruction :: ReplM m => InstructionPos -> m ()
    executeInstruction (InstructionPos i _ _) =
      case i of
        From (UntaggedImage img) -> do
          state <- containerId <$> get
          case state of
            Nothing -> do
              containerId <- (head . lines) <$>
                (liftIO $ readProcess "docker" ["run", "-dit", img] "")
              liftIO (putStrLn ("Started: " <> containerId))
              modify (\s -> s { containerId = (Just containerId) })
            Just containerId -> error "Process running"
        Run args -> do
          state <- containerId <$> get
          case state of
            Just containerId -> do
              liftIO $ callProcess "docker" (["exec", containerId] ++ args)
            Nothing -> error "Please start a container"
        Add src dest -> do
          state <- containerId <$> get
          case state of
            Just containerId -> do
              liftIO $ callProcess "docker" (["cp", src, containerId <> ":" <> dest])
            Nothing -> error "Please start a container"
        _ -> liftIO $ putStrLn "Not implemented"

main :: IO ()
main = do
    as <- getArgs
    case as of
        ("repl":_) -> replMain
        ("ipython":as) -> ipythonMain as
        _ -> error "Usage: idocker (repl|ipython) ...args"

ipythonMain :: [String] -> IO ()
ipythonMain args = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
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
