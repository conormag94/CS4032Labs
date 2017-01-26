{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryService (startApp) where

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
      return $ ["hello.txt", "test.txt"]

dirApi :: DP.Proxy DsAPI
dirApi = DP.Proxy

app :: Application
app = serve dirApi directoryServer






