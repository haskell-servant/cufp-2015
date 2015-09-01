{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module ChatApi where

import Servant
import Data.Aeson
import Data.Proxy
import GHC.Generics

newtype Person = Person { unPerson :: Int }
    deriving (Eq, Show, Generic, FromText, ToText)

data Message = SimpleMessage String
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON (SimpleMessage m) = toJSON m

instance FromJSON Message where
    parseJSON x = SimpleMessage <$> parseJSON x

type ChatApi
  = "from" :> Capture "person" Person :> ReqBody '[JSON] Message :> Post '[JSON] ()

chatApi :: Proxy ChatApi
chatApi = Proxy
