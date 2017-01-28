{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryService (DsAPI(..), startApp) where

import Prelude ()
import Prelude.Compat

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Proxy as DP
import Data.List
import GHC.Generics
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import System.Console.ANSI
import System.IO
import System.Directory
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

import FileServer

{-------------------------------------------------
     FileServer API - This module queries this API
-}------------------------------------------------

fileServerAPI :: DP.Proxy API
fileServerAPI = DP.Proxy

fileServerUrl :: BaseUrl
fileServerUrl = BaseUrl Http "localhost" 8080 ""

-- One function for each endpoint in the FileServer.hs API
getFile :: String -> ClientM FileObj
uploadFile :: FileObj -> ClientM ResponseMessage
deleteFile :: String -> ClientM ResponseMessage
modifyFile :: FileObj -> ClientM FileObj
listFiles :: ClientM [FilePath]
getReadmeFile :: ClientM FileObj

getFile :<|> uploadFile :<|> deleteFile :<|> modifyFile :<|> listFiles :<|> getReadmeFile = client fileServerAPI

{------------------------------------------------------
     DirectoryService API - This module serves this API
-}-----------------------------------------------------

type DsAPI = "findFile" :> Capture "filename" String :> Get '[JSON] ResponseMessage
        :<|> "listAll" :> Get '[JSON] [FilePath]

startApp :: IO ()
startApp = do 
  putStrLn "Directory server running on localhost:8081"
  run 8081 app

directoryServer :: Server DsAPI
directoryServer = findFile
             :<|> listAll

  where
    --Hardcoded file server location for now
    findFile :: String -> Handler ResponseMessage
    findFile fname = liftIO $ do
      return $ ResponseMessage {response = "localhost:8080"}

    listAll :: Handler [FilePath]
    listAll = liftIO $ do
      manager <- newManager defaultManagerSettings
      res <- runClientM (listFiles) (ClientEnv manager fileServerUrl)
      case res of
        Left err -> do 
          print err
          return []
        Right files -> do
          return files

dirApi :: DP.Proxy DsAPI
dirApi = DP.Proxy

app :: Application
app = serve dirApi directoryServer






