{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module LockService (startApp) where

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
import System.IO
import System.Directory
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

import FileServer

data FileLock = FileLock {
    fileName :: String
  , fileServer :: String
  , owner :: String
} deriving (Generic)

type LockAPI = "locktest" :> Get '[JSON] ResponseMessage

startApp :: IO ()
startApp = do 
  putStrLn "Lock server running on localhost:8082"
  run 8082 app

lockServer :: Server LockAPI
lockServer = locktest

  where
    locktest :: Handler ResponseMessage
    locktest = liftIO $ do
      return $ ResponseMessage {response = "Lock server up and running"}


lockApi :: DP.Proxy LockAPI
lockApi = DP.Proxy

app :: Application
app = serve lockApi lockServer