{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Reponse = Reponse
  { operator   :: String
  , arguments  :: [Double]
  , result     :: Double
  , error      :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Reponse)

startApp :: IO ()
startApp = run 8080 app

type API = Capture "action" String :> Capture "first" Double :> Capture "second" Double :> Get '[PlainText] String

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = handleAction

handleAction :: String -> Double -> Double -> Handler String
handleAction action first second = return (action ++ show first ++ show second)
