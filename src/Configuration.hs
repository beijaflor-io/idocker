{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Configuration where

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text
import           System.Environment
import           System.ZMQ4.Monadic

data Configuration = Configuration
  { ip               :: Text
  , control_port     :: Int
  , shell_port       :: Int
  , stdin_port       :: Int
  , hb_port          :: Int
  , transport        :: Text
  , signature_scheme :: Text
  , iopub_port       :: Int
  , key              :: Text
  } deriving (Show)

$(Aeson.deriveJSON Aeson.defaultOptions ''Configuration)
