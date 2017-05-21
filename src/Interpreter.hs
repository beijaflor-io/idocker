{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Interpreter where

import           Control.Concurrent.Lifted
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
import           Data.Aeson.Lens
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
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import           System.Process
import           System.ZMQ4.Monadic

type ContainerId = String
type ImageId = String
type SnapshotId = ImageId

type InterpreterInstruction = [(SnapshotId, InstructionPos)]

data InterpreterInteractiveProcess = InterpreterInteractiveProcess { iipStdin :: Maybe Handle
                                                                   , iipStdout :: Maybe Handle
                                                                   , iipStderr :: Maybe Handle
                                                                   , iipProcessHandle :: ProcessHandle
                                                                   }

data InterpreterContainer = InterpreterContainer { icId :: Maybe ContainerId
                                                 , icInstructions :: [InterpreterInstruction]
                                                 , icInteractiveProcess :: Maybe InterpreterInteractiveProcess
                                                 }

data InterpreterState = InterpreterState { isContainers :: [InterpreterContainer]
                                         }

type MonadInterpreter m =
  ( MonadError IOException m
  , MonadBaseControl IO m
  , MonadState InterpreterState m
  , MonadIO m
  )

data InterpreterCommand =
    ICDockerfile
    | ICGoto Int
    | ICExit
    | ICSaveFile FilePath
    | ICExecuteInstruction InstructionPos

containerId :: MonadInterpreter m => m (Maybe ContainerId)
containerId = undefined

prettyPrintII :: InterpreterInstruction -> String
prettyPrintII = prettyPrint . map snd

executeInstruction
  :: MonadInterpreter m =>
     InstructionPos -> m ()
executeInstruction ip@(InstructionPos i _ _) = run i
  where
    run i =
      case i of
        From (UntaggedImage img) -> do
          mcid <- containerId
          case mcid of
            Nothing -> do
              cid <-
                head . lines <$>
                liftIO (readProcess "docker" ["run", "-dit", img] "")
              liftIO (putStrLn ("Started: " <> cid))
                    -- modify (\s -> s {containerId = (Just containerId)})
            Just _ -> error "Process running"
        Run args -> do
          mcid <- containerId
          case mcid of
            Just cid -> liftIO $ callProcess "docker" (["exec", cid] ++ args)
            Nothing  -> error "Please start a container"
        Add src dest -> do
          mcid <- containerId
          case mcid of
            Just cid ->
              liftIO $ callProcess "docker" ["cp", src, cid <> ":" <> dest]
            Nothing -> error "Please start a container"
        _ -> liftIO $ putStrLn "Not implemented"

-- executeCommand
--   state <- containerId <$> get
--   case state of
--     Just containerId -> do
--         liftIO $ Just <$> readProcess "docker" (["commit", containerId]) ""
--     _ -> return Nothing

executeCommand
    :: MonadInterpreter m
    => InterpreterCommand -> m ()
executeCommand (ICGoto i) = do
  liftIO $ putStrLn "Stopping running container..."
  executeCommand ICExit
  liftIO $ putStrLn "Recreating..."
  s@InterpreterState {..} <- get
  let df = take i undefined -- replDockerfile
  let (sha, _) = last df
  cid <- liftIO (readProcess "docker" ["run", "-dit", sha] "")
  undefined -- put (s {replDockerfile = df, containerId = Just (head (lines cid))})
executeCommand (ICExecuteInstruction i) = do
    msha <- executeInstruction i
    case undefined of
        Just sha ->
            modify
            (\s ->
                 undefined
                 -- s
                -- { replDockerfile =
                --   replDockerfile s ++
                --   [(drop (length ("sha256:" :: String)) (head (lines sha)), i)]
                -- }
            )
        Nothing -> return ()
executeCommand ICExit = do
    mcontainerId <- containerId
    case mcontainerId of
        Just i ->
            liftIO $ do
            putStrLn "Exiting..."
            callProcess "docker" ["stop", i]
            callProcess "docker" ["rm", i]
        _ -> return ()
executeCommand (ICSaveFile fp) = do
    state <- undefined -- replDockerfile <$> get
    let df = prettyPrintII state
    liftIO $ writeFile "./Dockerfile" df
executeCommand ICDockerfile = do
    state <- undefined -- replDockerfile <$> get
    liftIO $ putStrLn (prettyPrintII state)
