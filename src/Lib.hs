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

data ResponseScheme = ResponseScheme
  { operator   :: String
  , arguments  :: [Double]
  , result     :: Maybe Double
  , error      :: Maybe String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ResponseScheme)

startApp :: IO ()
startApp = run 8000 app

type ActionApi = Capture "action" String :> Capture "first" Double :> Capture "second" Double :> Get '[JSON] ResponseScheme
type SqrtApi = "sqrt" :> Capture "value" Double :> Get '[JSON] ResponseScheme 

type API = ActionApi :<|>  SqrtApi

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = handleAction :<|> handleSqrt

handleAction :: String -> Double -> Double -> Handler ResponseScheme
handleAction "add" first second = return $ ResponseScheme "add" [first, second] (Just $ first + second) Nothing
handleAction "sub" first second = return $ ResponseScheme "sub" [first, second] (Just $ first - second) Nothing
handleAction "mul" first second = return $ ResponseScheme "mul" [first, second] (Just $ first * second) Nothing
handleAction "div" first second 
  | second == 0 = return $ ResponseScheme "div" [first, second] Nothing (Just "Division by zero")
  | otherwise   = return $ ResponseScheme "div" [first, second] (Just $ first / second) Nothing
handleAction "pow" first second
  | first == 0 && second < 0 = return $ ResponseScheme "pow" [first, second] Nothing (Just "Negative power of zero")
  | otherwise                = return $ ResponseScheme "pow" [first, second] (Just $ first ** second) Nothing

handleSqrt :: Double -> Handler ResponseScheme
handleSqrt value
  | value < 0 = return $ ResponseScheme "sqrt" [value] Nothing (Just "Sqrt value less than 0")
  | otherwise = return $ ResponseScheme "sqrt" [value] (Just $ sqrt value) Nothing