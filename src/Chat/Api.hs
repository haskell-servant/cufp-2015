{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Chat.Api where

import           Data.Aeson
import           Data.String
import           GHC.Generics
import           MarkdownCT
import           Servant
import           Servant.Docs

newtype Person = Person { name :: String }
    deriving (Eq, Show, Generic, FromText, ToText)

newtype Message = Message { unMessage :: String }
    deriving (Eq, Show, Generic, IsString, ToJSON, FromJSON)

type ChatApi =
       "docs" :> Get '[Markdown] Markdown
  :<|> "message" :> Capture "person" Person :> ReqBody '[JSON] Message :> Post '[JSON] ()
  :<|> "massages" :> QueryParam "offset" Int :> Get '[JSON] ([Message], Int)

instance ToSample Message Message where
    toSample _ = Just $ Message "hi, this is a message"

instance ToCapture (Capture "person" Person) where
    toCapture _ = DocCapture "person" "the person's name"

chatApi :: Proxy ChatApi
chatApi = Proxy
