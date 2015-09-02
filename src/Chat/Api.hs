{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Chat.Api where

import           Data.Aeson
import           GHC.Generics
import           MarkdownCT
import           Servant
import           Servant.Docs

newtype Person = Person { name :: String }
    deriving (Eq, Show, Generic, FromText, ToText)

data Message = SimpleMessage String
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON (SimpleMessage m) = toJSON m

instance FromJSON Message where
    parseJSON x = SimpleMessage <$> parseJSON x

type ChatApi =
       "docs" :> Get '[Markdown] Markdown
  :<|> "message" :> Capture "person" Person :> ReqBody '[JSON] Message :> Post '[JSON] ()
  :<|> "massages" :> QueryParam "offset" Int :> Get '[JSON] ([Message], Int)

instance ToSample Message Message where
    toSample _ = Just $ SimpleMessage "hi, this is a message"

instance ToCapture (Capture "person" Person) where
    toCapture _ = DocCapture "person" "the person's name"

chatApi :: Proxy ChatApi
chatApi = Proxy
