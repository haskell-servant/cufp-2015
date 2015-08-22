{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This file describes the API for the IP manager service we'll be using.
module CufpApi where

import           Data.Aeson
import           Data.Data
import           GHC.Generics
import           Servant
import           Servant.Docs

import           MarkdownCT

-- | An API with two endpoints.
type IpManager' =
       "nodes" :> Get '[JSON] [Node]
  :<|> "nodes" :> "new" :> ReqBody '[JSON] Node :> Post '[JSON] ()

-- | Combine the two endpoints o @IpManager'@ with another one for serving
-- documentation. This will be the entirety of the API we'll be using.
type IpManager = IpManager' :<|> "docs" :> Get '[Markdown] API

-- *  We frequently need proxies to guide instance selection of type classes.
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
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, ToJSON, FromJSON)


-- * 'ToSample' instances
--
-- 'ToSample' is a class for specifying a sample of a particular datatype,
-- which is used to generate documentation. Almost invariably, the first and
-- second type parameters will be the same.
instance ToSample Node Node where
    toSample _ = Just $ Node { host = "localhost", port = 8080 }

instance ToSample [Node] [Node] where
    toSample _ = Just [ Node { host = "localhost", port = 8080 }
                      , Node { host = "remote.com", port = 80 }
                      ]

instance ToSample () () where
    toSample _ = Just ()
