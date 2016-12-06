module Main where

--import Lib
import FileServer

main :: IO ()
main = run 8080 app
