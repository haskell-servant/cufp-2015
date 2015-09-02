{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatApp where

import           ChatApi
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.Monoid
import           MarkdownCT
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Servant.Docs

chatApp :: Server ChatApi
chatApp = apiDocs :<|> postMessage

personFile :: Person -> FilePath
personFile (Person p) = show p <> ".chat"

apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs chatApi

instance ToParam (QueryParam "answerPort" Int) where
  toParam Proxy = DocQueryParam "answerPort" ["8080", "9000"]
    "port where the sender hosts their own chat server" Normal

postMessage :: Person -> Message -> EitherT ServantErr IO ()
postMessage p (SimpleMessage msg) = liftIO $ appendFile (dataDir <> personFile p) msg

dataDir :: FilePath
dataDir = "chat/"

postMessage' :: Person -> Message -> EitherT ServantError IO ()
_ :<|> postMessage' = client chatApi (BaseUrl Http "localhost" 8087)

postMessage82 :: Person -> Message -> EitherT ServantError IO ()
_ :<|> postMessage82 = client chatApi (BaseUrl Http "jkarni.com" 8082)

main :: IO ()
main = do
    run 8087 $ serve chatApi chatApp
