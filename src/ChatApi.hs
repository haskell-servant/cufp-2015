{-# LANGUAGE DeriveAnyClass #-}
module ChatApi where

import Servant
import Data.Aeson
import Data.Proxy

newtype Person = Person { unPerson :: Int }
    deriving (Eq, Show, FromText, ToText)

data Message = SimpleMessage String
    deriving (Eq, Show, ToJSON)

type ChatApi
  = "to" :> Capture "person" Person :> ReqBody '[JSON] Message :> Post '[] ()

chatApi :: Proxy ChatApi
chatApi = Proxy
