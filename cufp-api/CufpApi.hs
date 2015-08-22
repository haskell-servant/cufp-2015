{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CufpApi where

import           Data.Aeson
import           Data.Data
import           GHC.Generics
import           Servant
import           Servant.Docs

import           MarkdownCT

type IpManager' =
       "nodes" :> Get '[JSON] [Node]
  :<|> "nodes" :> "new" :> ReqBody '[JSON] Node :> Post '[JSON] ()


type IpManager = IpManager' :<|> "docs" :> Get '[Markdown] API

ipManager :: Proxy IpManager
ipManager = Proxy

ipManager' :: Proxy IpManager'
ipManager' = Proxy

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

instance ToSample Node Node where
    toSample _ = Just $ Node { host = "localhost", port = 8080 }

instance ToSample [Node] [Node] where
    toSample _ = Just [ Node { host = "localhost", port = 8080 }
                      , Node { host = "remote.com", port = 80 }
                      ]

instance ToSample () () where
    toSample _ = Just ()
