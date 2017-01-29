{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Bson
import Data.Proxy as DP
import Data.List hiding (insert)
import Database.MongoDB
import Database.MongoDB
import GHC.Generics
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import System.IO
import System.Directory
import qualified Data.Text as DT
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

import FileServer

data FileLock = FileLock {
    fileName :: String
  , fileServer :: String
  , owner :: String
} deriving (Generic)

type LockAPI = "locktest" :> Get '[JSON] ResponseMessage

insertTest :: Document
insertTest = ["name" =: ("insertTest.txt" :: String), "data" =: ("It worked" :: String)]

startApp :: IO ()
startApp = do 
  putStrLn "Lock server running on localhost:8082"
  run 8082 app

lockServer :: Server LockAPI
lockServer = locktest

  where
    locktest :: Handler ResponseMessage
    locktest = liftIO $ do
      insertId <- withMongoDbConnection $ insert "locks" insertTest
      return $ ResponseMessage {response = "ID: " ++ (show insertId)}


lockApi :: DP.Proxy LockAPI
lockApi = DP.Proxy

app :: Application
app = serve lockApi lockServer

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  let ip = "127.0.0.1"
  let database = "locks"
  pipe <- connect (Database.MongoDB.host ip)
  ret <- runResourceT $ liftIO $ access pipe master (DT.pack database) act
  close pipe
  return ret

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if batch == []
        then return res
        else drainCursor' cur (res ++ batch)