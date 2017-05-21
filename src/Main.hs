{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

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
import           Repl

main :: IO ()
main = do
    as <- getArgs
    case as of
        ("repl":_)     -> replMain
        ("ipython":as) -> ipythonMain as
        _              -> error "Usage: idocker (repl|ipython) ...args"
