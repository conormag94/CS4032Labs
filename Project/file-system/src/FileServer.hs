{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module FileServer
  (startFileServer)
  where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
--import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


data File = File { name :: String
                 , body :: String
} deriving (Generic, FromJSON, ToJSON)



test1 :: File
test1 = File "test1.txt" "This is test file number 1"

test2 :: File
test2 = File "test2.txt" "The second test file"

fileList :: [File]
fileList = [test1, test2]

type FileApi = "files" :> Get '[JSON] [File]
          :<|> "test1" :> Get '[JSON] File
          :<|> "test2" :> Get '[JSON] String

startFileServer :: IO ()
startFileServer = run 8080 app

fileApi :: Proxy FileApi
fileApi = Proxy

server :: Server FileApi
server = return fileList
    :<|> return test1
    :<|> return (body test2)

app :: Application
app = serve fileApi server
