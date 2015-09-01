{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module ChatApi where

import Servant
import Servant.Docs
import Data.Aeson
import Data.Proxy
import GHC.Generics
import MarkdownCT

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
  :<|> "docs" :> Get '[Markdown] Markdown

instance ToSample Message Message where
    toSample _ = Just $ SimpleMessage "hi, this is a message"

instance ToCapture (Capture "person" Person) where
    toCapture _ = DocCapture "person" "the person's id"

chatApi :: Proxy ChatApi
chatApi = Proxy
