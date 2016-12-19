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

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

data File = File { name :: String
                 , content :: TL.Text
} deriving (Generic)

instance ToJSON File
instance FromJSON File

testFile1 :: File
testFile1 = File "test1.txt" "This is test file number 1"

testFile2 :: File
testFile2 = File "test2.txt" "The second test file"

fileList :: [File]
fileList = [testFile1, testFile2]

type API = "files" :> Get '[JSON] [File]
      :<|> "test1" :> Get '[JSON] File
      :<|> "getReadme" :> Get '[JSON] File

startFileServer :: IO ()
startFileServer = run 8080 app

server :: Server API
server = files
    :<|> test1
    :<|> getReadme

  where files :: Handler [File]
        files = return fileList

        test1 :: Handler File
        test1 = return testFile1

        getReadme :: Handler File
        getReadme = liftIO $ do 
          contents <- TextIO.readFile "./static-files/test.txt" 
          return $ File "test.txt" contents


fileApi :: Proxy API
fileApi = Proxy

app :: Application
app = serve fileApi server
