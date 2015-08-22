{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module IpApi where

import           Data.Aeson
import           Data.Data
import           GHC.Generics
import           Servant

type IpManager =
       "nodes" :> Get '[JSON] [Node]
  :<|> "nodes" :> "new" :> ReqBody '[JSON] Node :> Post '[JSON] ()

ipManager :: Proxy IpManager
ipManager = Proxy

data Node
  = Node {
    host :: String,
    port :: Int
  }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance ToJSON Node

instance FromJSON Node
