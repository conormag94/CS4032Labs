{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Client
    ( startClient
    ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Proxy as DP
import Data.List
import Data.List.Split
import GHC.Generics
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import System.Console.ANSI
import System.IO
import System.Directory
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

import FileServer
import DirectoryService
--import Lib

userDirectory = "./user-files/"

-- Colour codes from Stephen's sample project
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
greenCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green]
yellowCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Yellow]
resetCode = setSGRCode [Reset]

{-----------------------------------------------
    FileServer API - Queried by this module
-}----------------------------------------------

-- API imported from FileServer.hs in the file-system project
fsApi :: DP.Proxy API
fsApi = DP.Proxy

-- Only looks at one file server for now
fileServerUrl :: BaseUrl
fileServerUrl = BaseUrl Http "localhost" 8080 ""

-- One function for each endpoint in the FileServer.hs API
get :: String -> ClientM FileObj
upload :: FileObj -> ClientM ResponseMessage
delete :: String -> ClientM ResponseMessage
modify :: FileObj -> ClientM FileObj
list :: ClientM [FilePath]
getReadme :: ClientM FileObj

get :<|> upload :<|> delete :<|> modify :<|> list :<|> getReadme = client fsApi

{-----------------------------------------------
    DirectoryServer API - Queried by this module
-}----------------------------------------------

dsApi :: DP.Proxy DsAPI
dsApi = DP.Proxy

dirServerUrl :: BaseUrl
dirServerUrl = BaseUrl Http "localhost" 8081 ""

-- One function for each endpoint in the DirectoryService.hs API
whereFile :: String -> ClientM ResponseMessage
listFiles :: ClientM [FilePath]

whereFile :<|> listFiles = client dsApi


extractFileServer :: ResponseMessage -> BaseUrl
extractFileServer resp = 
  let server = splitOn ":" (response resp)
      host = (server !! 0)
      port = (server !! 1)
  in  BaseUrl Http host (read port :: Int) ""


greeting :: IO ()
greeting = do
  putStrLn $ blueCode ++ "=== Distributed File System Client ==="
  putStrLn $ yellowCode ++ "Usage: open, upload, put, delete, list, quit, help" ++ resetCode

displayHelp :: IO ()
displayHelp = do
  putStrLn "Commands"
  putStrLn "========"
  putStrLn "open <file>          - Open the file '<file>' from server and store cached copy"
  putStrLn "close <file>         - Upload your changes to the file '<file>' to server and delete cached copy"
  putStrLn "read <file>          - Read the contents of the locally cached version of <file>"
  putStrLn "write <file> [data]  - Write <data> to the locally cached version of <file>"
  putStrLn "list all             - List all remote files"
  putStrLn "list cached          - List all local cached files"
  putStrLn "upload <file>        - Upload the file '<file>' to server"
  putStrLn "put <file>           - Modify the file '<file>' on server"
  putStrLn "delete <file>        - Delete the file '<file>' from server"
  putStrLn "quit                 - Quit the client"


parseInput :: String -> [String] -> IO ()
parseInput "open" (arg:args) = do
  putStrLn $ "Opening file: " ++ arg
  -- First, ask directory server where the file is
  manager <- newManager defaultManagerSettings
  loc <- runClientM (whereFile arg) (ClientEnv manager dirServerUrl)
  case loc of
    Left err -> putStrLn $ redCode ++ "Error: " ++ show err
    Right response -> do
      --Turn server:port response into a BaseUrl to get the file from
      let fServer = extractFileServer response
      res <- runClientM (get arg) (ClientEnv manager fServer)
      case res of
        Left err -> putStrLn $ redCode ++ "Error: " ++ show err
        Right f -> do
          let fpath = userDirectory ++ (name f)
          fileExists <- doesFileExist fpath
          case fileExists of
            True -> do
              putStrLn $ yellowCode ++ "Cached version of file already present"
            False -> do
              TextIO.writeFile fpath (content f)
              putStrLn $ greenCode ++ (name f) ++ " downloaded to " ++ userDirectory
  prompt

parseInput "close" (file:_) = do
  let fpath = (userDirectory ++ file)
  fileExists <- doesFileExist fpath
  case fileExists of
    True -> do 
      fcontent <- TextIO.readFile fpath
      let fileObj = FileObj file fcontent
      manager <- newManager defaultManagerSettings
      res <- runClientM (upload fileObj) (ClientEnv manager fileServerUrl)
      case res of 
        Left err -> do 
          putStrLn $ redCode ++ "Unable to upload file"
          putStrLn "Perhaps the server's copy of the file has been updated since you opened it"
          putStrLn $ yellowCode ++ "You must download the new copy"
        Right resp -> do
          removeFile fpath
          putStrLn $ greenCode ++ fpath ++ " uploaded and uncached"
  prompt

parseInput "read" (file:_) = do
  let fpath = (userDirectory ++ file)
  fileExists <- doesFileExist fpath
  case fileExists of
    True -> do
      fcontent <- TextIO.readFile fpath
      putStrLn $ yellowCode ++ "File " ++ file ++ ": " ++ resetCode
      putStrLn (show fcontent)
    False -> do
      putStrLn $ redCode ++ "File " ++ fpath ++ " not found"
      putStrLn $ yellowCode ++ "Perhaps you need to open it from the file server?"
  prompt

parseInput "write" (file:newContent) = do
  let fpath = (userDirectory ++ file)
  fileExists <- doesFileExist fpath
  case fileExists of
    True -> do
      TextIO.writeFile fpath (TL.pack $ unwords newContent)
      putStrLn $ greenCode ++ "File " ++ file ++ " written"
    False -> do
      putStrLn $ redCode ++ "File " ++ fpath ++ " not found"
      putStrLn $ yellowCode ++ "Perhaps you need to open it from the file server?"
  prompt

parseInput "upload" (arg:args) = do
  putStrLn $ "Uploading file: " ++ arg
  let fpath = (userDirectory ++ arg)
  fileExists <- doesFileExist fpath
  case fileExists of
    True -> do
      putStrLn "File Exists"
      fcontent <- TextIO.readFile fpath
      let fileObj = FileObj arg fcontent
      manager <- newManager defaultManagerSettings
      res <- runClientM (upload fileObj) (ClientEnv manager fileServerUrl)
      putStrLn $ greenCode ++ "It worked I think" ++ resetCode
      putStrLn $ (show res)
    False -> do
      putStrLn $ redCode ++ "File does not exist"
  prompt

parseInput "put" args = do
  putStrLn "You want to modify a file"
  prompt

parseInput "delete" args = do
  putStrLn "You want to delete something"
  prompt

parseInput "list" ("cached":_) = do
  file_list <- listDirectory "user-files"
  putStrLn $ yellowCode ++ "Cached files:" ++ resetCode
  putStr $ unlines file_list
  prompt

parseInput "list" ("all":_) = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (listFiles) (ClientEnv manager dirServerUrl)
  case res of
    Left err -> putStrLn $ redCode ++ "Error: " ++ show err
    Right files -> do
      putStrLn $ yellowCode ++ "All files:" ++ resetCode
      putStr $ unlines files
  prompt

parseInput "quit" _ = do
  putStrLn "Exiting..."
  return ()

parseInput "help" _ = do
  displayHelp
  prompt

parseInput c _ = do
  putStrLn $ redCode ++ "'" ++ c ++ "'" ++ " not recognised"
  prompt

-- Reads a line of input and sends to the parseInput function
-- Input in the form : <command> [<arguments>]
-- E.g: get sample/file/path/file.txt
prompt :: IO ()
prompt = do
  putStr $ blueCode ++ ">> " ++ resetCode
  hFlush stdout
  userInput <- getLine
  let commandWords = words userInput
  parseInput (head commandWords) (tail commandWords)

-- Entry point for the Client
-- Displays initial greeting then calls the recursive prompt function
startClient :: IO ()
startClient = do 
  greeting
  prompt

