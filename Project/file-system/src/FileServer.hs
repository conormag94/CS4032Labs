{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module FileServer(
  API(..), FileObj(..), ResponseMessage(..), startFileServer)
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
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Server
import System.Directory
import Servant.Client
import qualified Data.Aeson.Parser
import Data.Proxy as DP
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

import LockService

{-------------------------------------------------
     FileServer API - This module serves this API
-}------------------------------------------------

baseDirectory = "./static-files/"

data FileObj = FileObj { name :: String
                 , content :: TL.Text
} deriving (Generic)

instance ToJSON FileObj
instance FromJSON FileObj

instance Show FileObj where
  show (FileObj n c) = ("{ " ++ (show n) ++ "\n" ++ (show c) ++ " }") 

data ResponseMessage = ResponseMessage { response :: String } deriving(Generic)

instance ToJSON ResponseMessage
instance FromJSON ResponseMessage

instance Show ResponseMessage where
  show (ResponseMessage r) = show r

--TODO: Figure out the proper HTTP verbs and proper response types for each endpoint
type API = "files" :> Capture "filename" String :> Get '[JSON] FileObj
      :<|> "upload" :> ReqBody '[JSON] FileObj :> Post '[JSON] ResponseMessage
      :<|> "delete" :> Capture "file" String :> Get '[JSON] ResponseMessage
      :<|> "modify" :> ReqBody '[JSON] FileObj :> Post '[JSON] FileObj
      :<|> "list" :> Get '[JSON] [FilePath]
      :<|> "getReadme" :> Get '[JSON] FileObj

{-------------------------------------------------
     LockService API - This module queries this API
-}------------------------------------------------

lockServiceAPI :: DP.Proxy LockAPI
lockServiceAPI = DP.Proxy

lockServiceUrl :: BaseUrl
lockServiceUrl = BaseUrl Http "localhost" 8082 ""

-- One function for each endpoint in the FileServer.hs API
lockF :: FileLock -> ClientM LockResult
unlockF :: FileLock -> ClientM LockResult
checkF :: FileLock -> ClientM LockResult

lockF :<|> unlockF :<|> checkF = client lockServiceAPI

{-------------------------------------------------
     The File Server
-}------------------------------------------------

startFileServer :: IO ()
startFileServer = do 
  putStrLn "Running on localhost:8080"
  run 8080 app

--TODO: Figure out error codes instead of manually sending error messages
server :: Server API
server = getFile
    :<|> uploadFile
    :<|> deleteFile
    :<|> modifyFile
    :<|> listFiles
    :<|> getReadme

  where 
    getFile :: String -> Handler FileObj
    getFile fname = liftIO $ do
      let fpath = baseDirectory ++ fname
      _content <- TextIO.readFile fpath
      return $ FileObj {name = fname, content = _content}

    uploadFile :: FileObj -> Handler ResponseMessage
    uploadFile nf = liftIO $ do
      let fpath = baseDirectory ++ (name nf)
      TextIO.writeFile fpath (content nf)
      return $ ResponseMessage {response = "File written"}

    deleteFile :: String -> Handler ResponseMessage
    deleteFile fname = liftIO $ do
      fileExists <- doesFileExist (baseDirectory ++ fname)
      case fileExists of
        True -> do 
          (removeFile (baseDirectory ++ fname))
          return $ ResponseMessage {response = (fname ++ " deleted")}
        False -> do
          return $ ResponseMessage {response = (fname ++ " not found on server")}

    --TODO: figure out a better way of writing and returning a response
    modifyFile :: FileObj -> Handler FileObj
    modifyFile f = liftIO $ do
      let fpath = baseDirectory ++ (name f)
      fileExists <- doesFileExist fpath
      case fileExists of
        True -> do
          TextIO.writeFile fpath (content f)
          return $ FileObj {name = (name f), content = (content f)}
        False -> do
          return $ FileObj {name = "", content = "File not found"}

    listFiles :: Handler [FilePath]
    listFiles = liftIO $ do
      file_list <- listDirectory "static-files"
      return $ file_list


    getReadme :: Handler FileObj
    getReadme = liftIO $ do 
      contents <- TextIO.readFile "./static-files/test.txt" 
      return $ FileObj "test.txt" contents


fileApi :: DP.Proxy API
fileApi = DP.Proxy

app :: Application
app = serve fileApi server
