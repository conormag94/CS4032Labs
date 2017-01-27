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

-- API imported from FileServer.hs in the file-system project
fsApi :: DP.Proxy API
fsApi = DP.Proxy

serverUrl :: BaseUrl
serverUrl = BaseUrl Http "localhost" 8080 ""

-- One function for each endpoint in the FileServer.hs API
get :: String -> ClientM FileObj
upload :: FileObj -> ClientM ResponseMessage
delete :: String -> ClientM ResponseMessage
modify :: FileObj -> ClientM FileObj
list :: ClientM [FilePath]
getReadme :: ClientM FileObj

get :<|> upload :<|> delete :<|> modify :<|> list :<|> getReadme = client fsApi

-- Colour codes from Stephen's sample project
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
greenCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green]
yellowCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Yellow]
resetCode = setSGRCode [Reset]

greeting :: IO ()
greeting = do
  putStrLn $ blueCode ++ "=== Distributed File System Client ==="
  putStrLn $ yellowCode ++ "Usage: get, post, put, delete, quit, help" ++ resetCode

displayHelp :: IO ()
displayHelp = do
  putStrLn "Commands"
  putStrLn "========"
  putStrLn "get <filename>    - Get the file '<filename>' from server"
  putStrLn "post <filename>   - Upload the file '<filename>' to server"
  putStrLn "put <filename>    - Modify the file '<filename>' on server"
  putStrLn "delete <filename> - Delete the file '<filename>' from server"
  putStrLn "quit              - Quit the client"

-- Get and post implemented. The rest is skeleton code for now
parseInput :: String -> [String] -> IO ()
parseInput "get" (arg:args) = do
  putStrLn $ "Getting file: " ++ arg
  manager <- newManager defaultManagerSettings
  res <- runClientM (get arg) (ClientEnv manager serverUrl)
  putStrLn $ "Result:" ++ greenCode
  case res of
    Left err -> putStrLn $ redCode ++ "Error: " ++ show err
    Right f -> do
      let fpath = userDirectory ++ (name f)
      TextIO.writeFile fpath (content f)
      putStrLn $ (name f) ++ " downloaded to " ++ userDirectory
  prompt

parseInput "post" (arg:args) = do
  putStrLn $ "Uploading file: " ++ arg
  let fpath = (userDirectory ++ arg)
  fileExists <- doesFileExist fpath
  case fileExists of
    True -> do
      putStrLn "File Exists"
      fcontent <- TextIO.readFile fpath
      let fileObj = FileObj arg fcontent
      manager <- newManager defaultManagerSettings
      res <- runClientM (upload fileObj) (ClientEnv manager serverUrl)
      putStrLn $ greenCode ++ "It worked I think" ++ resetCode
      putStrLn $ (show res)
    False -> do
      putStrLn $ redCode ++ "File does not exist"
  prompt

parseInput "put" args = do
  putStrLn "Put it there"
  prompt

parseInput "delete" args = do
  putStrLn "You want to delete something"
  prompt

parseInput "quit" _ = do
  putStrLn "Exiting..."
  return ()

parseInput "help" _ = do
  displayHelp
  prompt

parseInput c _ = do
  putStrLn $ redCode ++ "'" ++ c ++ "'" ++ " not recognised"
  -- putStrLn $ yellowCode ++ "Usage: get, post, put, delete" ++ resetCode
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

