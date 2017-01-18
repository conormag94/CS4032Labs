{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Client
    ( startClient
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy as DP
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import System.Console.ANSI
import System.IO

import FileServer

-- API imported from FileServer.hs in the file-system project
fsApi :: DP.Proxy API
fsApi = DP.Proxy

serverUrl :: BaseUrl
serverUrl = BaseUrl Http "localhost" "8080" ""

-- One function for each endpoint in the FileServer.hs API
get :: Maybe String -> ClientM File
upload :: File -> ClientM ResponseMessage
delete :: String -> ClientM ResponseMessage
modify :: File -> ClientM File
getReadme :: ClientM File

(get :<|> upload :<|> delete :<|> modify :<|> getReadme) = client fsApi

-- Colour codes from Stephen's sample project
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
greenCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green]
yellowCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Yellow]
resetCode = setSGRCode [Reset]

getFile :: String -> ClientM FileObject
getRequest fPath = do
  f <- get (Just fPath)
  return f

greeting :: IO ()
greeting = do
  putStrLn $ blueCode ++ "=== Distributed File System Client ==="
  putStrLn $ yellowCode ++ "Usage: get, post, put, delete, quit" ++ resetCode

parseInput :: String -> [String] -> IO ()
parseInput "get" (arg:args) = do
  putStrLn $ "Getting file: " ++ arg
  manager <- newManager defaultManagerSettings
  res <- runClientM (getFile arg) (ClientEnv manager serverUrl)
  case res of
    Left err -> putStrLn $ redCode ++ "Error: " ++ (show err)
    Right f -> do
      putStrLn $ show (content f)
  prompt

parseInput "post" args = do
  putStrLn "Postman pat"
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

parseInput _ _ = do
  putStrLn $ redCode ++ "Not a valid command"
  putStrLn $ yellowCode ++ "Usage: get, post, put, delete" ++ resetCode
  prompt

-- Reads a line of input and sends to the parseInput function
-- Input in the form : <command> [<arguments>]
-- E.g: get sample/file/path/file.txt
prompt :: IO ()
prompt = do
  putStr ">> "
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

