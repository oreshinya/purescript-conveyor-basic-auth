module Conveyor.BasicAuth where

import Prelude

import Conveyor.Servable (class Servable, serve)
import Conveyor.Types (Responder(..))
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Node.BasicAuth (Credentials, authenticate)



data BasicAuth server = BasicAuth Credentials server

instance servableBasicAuth :: Servable ex server => Servable ex (BasicAuth server) where
  serve (BasicAuth cred server) extraData rawData = do
    ok <- liftEffect $ authenticate cred rawData.req
    if ok
      then
        serve server extraData rawData
      else
        pure $ Responder
          { contentType: "text/plain; charset=utf-8"
          , code: 401
          , body: unsafeToForeign ""
          }
