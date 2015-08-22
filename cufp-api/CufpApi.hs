{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module CufpApi where

import           Data.Aeson
import           Data.Data
import           GHC.Generics
import           Servant

type IpManager =
       "nodes" :> Get '[JSON] [Node]
  :<|> "nodes" :> "new" :> ReqBody '[JSON] Node :> Post '[JSON] ()

ipManager :: Proxy IpManager
ipManager = Proxy

type Host = String
type Port = Int

data Node
  = Node {
    host :: Host,
    port :: Port
  }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance ToJSON Node

instance FromJSON Node
