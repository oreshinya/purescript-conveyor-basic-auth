module Conveyor.BasicAuth where

import Prelude

import Conveyor.Servable (class Servable, serve)
import Conveyor.Types (Responder(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Node.BasicAuth (Credentials, authenticate)
import Node.HTTP (setHeader)
import Simple.JSON (write)



type Settings =
  { realm :: String
  , credentials :: Credentials
  }

data BasicAuth server = BasicAuth (Maybe Settings) server

instance servableBasicAuth :: Servable ex server => Servable ex (BasicAuth server) where
  serve (BasicAuth mSetting server) extraData rawData =
    case mSetting of
      Nothing -> serve server extraData rawData
      Just s -> do
        ok <- liftEffect $ authenticate s.credentials rawData.req
        if ok
          then serve server extraData rawData
          else do
            liftEffect $ setHeader rawData.res "WWW-Authenticate" $ "Basic realm=\"" <> s.realm <> "\""
            pure $ Responder
              { contentType: "application/json; charset=utf-8"
              , code: 401
              , body: write { message: "Unauthorized" }
              }

