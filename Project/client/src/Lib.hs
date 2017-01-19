{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.List
import GHC.Generics
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TextIO

{-

  Copied the API here until I figure out how to import it properly from the file-server project

-}
data FileObj = FileObj { name :: String
                 , content :: TL.Text
} deriving (Generic)

instance ToJSON FileObj
instance FromJSON FileObj

data ResponseMessage = ResponseMessage { response :: String } deriving(Generic)

instance ToJSON ResponseMessage
instance FromJSON ResponseMessage

--TODO: Figure out the proper HTTP verbs and proper response types for each endpoint
type API = "files" :> Capture "filename" String :> Get '[JSON] FileObj
      :<|> "upload" :> ReqBody '[JSON] FileObj :> Post '[JSON] ResponseMessage
      :<|> "delete" :> Capture "file" String :> Get '[JSON] ResponseMessage
      :<|> "modify" :> ReqBody '[JSON] FileObj :> Post '[JSON] FileObj
      :<|> "getReadme" :> Get '[JSON] FileObj
