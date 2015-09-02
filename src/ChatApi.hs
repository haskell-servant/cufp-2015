{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module ChatApi where

import           Data.Aeson
import           GHC.Generics
import           MarkdownCT
import           Servant
import           Servant.Docs

newtype Person = Person { unPerson :: Int }
    deriving (Eq, Show, Generic, FromText, ToText)

data Message = SimpleMessage String
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON (SimpleMessage m) = toJSON m

instance FromJSON Message where
    parseJSON x = SimpleMessage <$> parseJSON x

type ChatApi =
       "docs" :> Get '[Markdown] Markdown
  :<|> "from" :> Capture "person" Person :> ReqBody '[JSON] Message :> Post '[JSON] ()

instance ToSample Message Message where
    toSample _ = Just $ SimpleMessage "hi, this is a message"

instance ToCapture (Capture "person" Person) where
    toCapture _ = DocCapture "person" "the person's id"

chatApi :: Proxy ChatApi
chatApi = Proxy
