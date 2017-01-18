{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Client
    ( startClient
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Console.ANSI

import FileServer

redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
greenCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green]
yellowCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Yellow]
resetCode = setSGRCode [Reset]

greeting :: IO ()
greeting = do
  putStrLn $ blueCode ++ "=== Distributed File System Client ==="
  putStrLn $ yellowCode ++ "Usage: get, post, put, delete" ++ resetCode

parseInput :: String -> [String] -> IO ()
parseInput "get" args = do
  putStrLn "You want to get got"

parseInput "post" args = do
  putStrLn "Postman pat"

parseInput "put" args = do
  putStrLn "Put it there"

parseInput "delete" args = do
  putStrLn "You want to delete something"

parseInput _ _ = do
  putStrLn $ redCode ++ "Not a valid command"
  putStrLn $ yellowCode ++ "Usage: get, post, put, delete" ++ resetCode

startClient :: IO ()
startClient = do 
  greeting
  userInput <- getLine
  let commandWords = words userInput
  parseInput (head commandWords) (tail commandWords)

