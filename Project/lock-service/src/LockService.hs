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

module LockService (LockAPI(..), FileLock(..), LockResult(..), startApp) where

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

data LockResult = LockResult { 
    result :: String
  , message :: String 
} deriving(Generic, Show, Read)

instance ToJSON LockResult
instance FromJSON LockResult

data FileLock = FileLock {
    fileName :: String
  , fileServer :: String
  , owner :: String
} deriving (Generic, Show, Read)

instance ToJSON FileLock
instance FromJSON FileLock

type LockAPI = "lockFile" :> ReqBody '[JSON] FileLock :> Post '[JSON] LockResult
          :<|> "unlockFile" :> ReqBody '[JSON] FileLock :> Delete '[JSON] LockResult
          :<|> "checkLock" :> ReqBody '[JSON] FileLock :> Get '[JSON] LockResult


lockToDoc :: FileLock -> Document
lockToDoc (FileLock {fileName = name, fileServer = server, owner = o}) = 
  ["fileName" =: name, "fileServer" =: server, "owner" =: o]

-- isFileLocked :: FileLock -> Action IO Bool
-- isFileLocked (FileLock {fileName = name, fileServer = server, owner = o}) = do
--   let selector = ["fileName" =: name, "fileServer" =: server, "owner" =: o]
--   lockStatus <- withMongoDbConnection $ findOne $ select selector "locks"
--   case lockStatus of 
--     Nothing -> return False
--     Just _ -> return True

-- Returns the Value of a Field in a Document
-- From http://stevepowell.ca/mongo-haskell.html
extractString :: Label -> Document -> String
extractString label = typed . (valueAt label)

startApp :: IO ()
startApp = do 
  putStrLn "Lock server running on localhost:8082"
  run 8082 app

lockServer :: Server LockAPI
lockServer = lockFile
        :<|> unlockFile
        :<|> checkLock

  where

    lockFile :: FileLock -> Handler LockResult
    lockFile fl = liftIO $ do
      let selector = ["fileName" =: (fileName fl), "fileServer" =: (fileServer fl)]
      lockStatus <- withMongoDbConnection $ findOne $ select selector "locks"
      -- Is FileLock already in DB?
      case lockStatus of
        -- No -> Add it to DB, locking the file
        Nothing -> do 
          let flDoc = lockToDoc fl
          insertId <- withMongoDbConnection $ insert "locks" flDoc
          return $ LockResult {result = "SUCCESS",  message = ((fileName fl) ++ " locked")}
        -- Yes -> Return an error
        Just l -> do
          let lockOwner = extractString "owner" l
          let fname = extractString "fileName" l
          return $ LockResult {result = "ERROR", message = (fname ++ " already locked by " ++ lockOwner)}
      

    unlockFile :: FileLock -> Handler LockResult
    unlockFile fl = liftIO $ do
      let selector = ["fileName" =: (fileName fl), "fileServer" =: (fileServer fl)]
      lockStatus <- withMongoDbConnection $ findOne $ select selector "locks"
      -- Is FileLock already in DB?
      case lockStatus of
        -- No -> No lock to unlock, return an error
        Nothing -> do
          return $ LockResult {result = "ERROR", message = ((fileName fl) ++ " already unlocked")}
        -- Yes -> Does user have permission to unlock?
        Just l -> do
          let lockOwner = extractString "owner" l
          case ((owner fl) == lockOwner) of
            True -> do
              deleteStatus <- withMongoDbConnection $ deleteOne $ select selector "locks"
              return $ LockResult {result = "SUCCESS", message =  ((fileName fl) ++ " unlocked")}
            False -> do
              return $ LockResult {result = "ERROR", message = ((fileName fl) ++ " locked by " ++ lockOwner ++ ". Only they can unlock it")}

      -- return $ LockResult (show fl)

    checkLock :: FileLock -> Handler LockResult
    checkLock (FileLock name server _owner) = liftIO $ do
      let selector = ["fileName" =: name, "fileServer" =: server]
      lockStatus <- withMongoDbConnection $ findOne $ select selector "locks"
      case lockStatus of 
        Nothing -> 
          return $ LockResult {result = "UNLOCKED", message = ""}
        Just l -> do
          let lockOwner = extractString "owner" l
          return $ LockResult {result = "LOCKED", message = lockOwner}
          -- print lockOwner
          -- print _owner
          -- case (lockOwner == _owner) of
          --   True -> return $ LockResult $ "LOCKED"
          --   False -> return $ LockResult $ "File is locked by someone else"

lockApi :: DP.Proxy LockAPI
lockApi = DP.Proxy

app :: Application
app = serve lockApi lockServer


{-------------------------------------------------
    MongoDB helper functions (from Use-Haskell)
-}------------------------------------------------

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