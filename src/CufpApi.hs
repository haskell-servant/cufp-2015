{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This file describes the API for the IP manager service we'll be using.
module CufpApi
  ( module CufpApi
  , Markdown(..)
  ) where


import           Data.Aeson
import           Data.Data
import           GHC.Generics
import           Servant.API
import           Servant.Docs

import           MarkdownCT

-- | An API with two endpoints and documentation for them.
type CufpApi =
       "nodes" :> Get '[JSON] [Node]
  :<|> "nodes" :> "new" :> ReqBody '[JSON] Node :> Post '[JSON] ()
  :<|> "docs" :> Get '[Markdown] Markdown
  :<|> "nodes" :> "delete" :> ReqBody '[JSON] Node :> Delete '[JSON] ()


-- *  We frequently need proxies to guide instance selection of type classes.
cufpApi :: Proxy CufpApi
cufpApi = Proxy

data Node
  = Node {
    host :: String,
    port :: Int
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

