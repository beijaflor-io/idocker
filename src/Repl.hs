{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

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

import           Configuration
import           Kernel

replMain :: IO ()
replMain = do
    _ <- runStateT runRepl (ReplState Nothing [])
    return ()

data ReplState = ReplState { containerId    :: Maybe String
                           , replDockerfile :: [(String, InstructionPos)]
                           }
type ReplM m = (MonadError IOException m, MonadBaseControl IO m, MonadState ReplState m, MonadIO m)

data ReplCommand = ReplCommandDockerfile
                 | ReplCommandGoto Int
                 | ReplCommandExit
                 | ReplCommandFlush
                 | ReplCommandEOL
                 | ReplCommandInstruction InstructionPos

replInstructionCount = length . replDockerfile

runRepl :: ReplM m => m ()
runRepl = do
  run `catchError` (\e -> liftIO (print e))
  runRepl
  where
    prettyPrint' = prettyPrint . map snd
    run = readCommand >>= executeCommand
    getPrompt
      :: ReplM m
      => m String
    getPrompt = do
      iCount <- replInstructionCount <$> get
      currentSha <-
        do df <- replDockerfile <$> get
           case df of
             (_:_) -> return (fst (last df))
             []    -> return "<none>"
      let c =
            [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
      return $
        c `colored` ("[" <> show iCount <> ":" <> take 5 currentSha <> "] ")
      where
        colored c s = prefix <> s <> suffix
          where
            prefix = setSGRCode c
            suffix = setSGRCode [Reset]
        prefix = setSGRCode [SetColor Foreground Vivid Blue]
        suffix = setSGRCode [Reset]
    readCommand
      :: ReplM m
      => m ReplCommand
    readCommand = do
      prompt <- getPrompt
      ln <- liftIO $ runInputT defaultSettings $ loop prompt
      let einstructions = Dockerfile.parseString ln
      case einstructions of
        Left err
            -- liftIO $ print err
         -> do
          case words ln of
            ["GOTO", ln] -> return (ReplCommandGoto (read ln))
            ["DOCKERFILE"] -> return ReplCommandDockerfile
            ["FLUSH"] -> return ReplCommandFlush
            ["EXIT"] -> return ReplCommandExit
            _ ->
              return
                (ReplCommandInstruction
                   (InstructionPos (Run (words ln)) "Dockerfile" 0))
        Right (i:_) -> do
          return (ReplCommandInstruction i)
        _ -> return ReplCommandEOL
      where
        loop prompt = do
          mln <- getInputLine prompt
          case mln of
            Nothing -> loop prompt
            Just ln -> return ln
    executeCommand
      :: ReplM m
      => ReplCommand -> m ()
    executeCommand (ReplCommandGoto i) = do
      liftIO $ putStrLn "Stopping running container..."
      executeCommand ReplCommandExit
      liftIO $ putStrLn "Recreating..."
      s@ReplState {..} <- get
      let df = take i replDockerfile
      let (sha, _) = last df
      cid <- liftIO (readProcess "docker" ["run", "-dit", sha] "")
      put (s {replDockerfile = df, containerId = Just (head (lines cid))})
    executeCommand (ReplCommandInstruction i) = do
      msha <- executeInstruction i
      case msha of
        Just sha ->
          modify
            (\s ->
               s
               { replDockerfile =
                   replDockerfile s ++
                   [(drop (length ("sha256:" :: String)) (head (lines sha)), i)]
               })
        Nothing -> return ()
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
      let df = prettyPrint' state
      liftIO $ writeFile "./Dockerfile" df
    executeCommand ReplCommandDockerfile = do
      state <- replDockerfile <$> get
      liftIO $ putStrLn (prettyPrint' state)
    executeInstruction
      :: ReplM m
      => InstructionPos -> m (Maybe String)
    executeInstruction ip@(InstructionPos i _ _) = do
      liftIO $ putStrLn (prettyPrint [ip])
      case i of
        From (UntaggedImage img) -> do
          state <- containerId <$> get
          case state of
            Nothing -> do
              containerId <-
                (head . lines) <$>
                (liftIO $ readProcess "docker" ["run", "-dit", img] "")
              liftIO (putStrLn ("Started: " <> containerId))
              modify (\s -> s {containerId = (Just containerId)})
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
              liftIO $
                callProcess "docker" (["cp", src, containerId <> ":" <> dest])
            Nothing -> error "Please start a container"
        _ -> liftIO $ putStrLn "Not implemented"
      state <- containerId <$> get
      case state of
        Just containerId -> do
          liftIO $ Just <$> readProcess "docker" (["commit", containerId]) ""
        _ -> return Nothing
