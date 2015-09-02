{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chat.Server where

import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.Maybe
import           MarkdownCT
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Docs
import qualified System.Logging.Facade as Log

import           Chat.Api

chatApp :: MVar [Message] -> Server ChatApi
chatApp mvar = apiDocs :<|> postMessage mvar :<|> getMessages mvar

apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs chatApi

instance ToParam (QueryParam "answerPort" Int) where
  toParam Proxy = DocQueryParam "answerPort" ["8080", "9000"]
    "port where the sender hosts their own chat server" Normal

instance ToSample ([Message], Int) ([Message], Int) where
  toSample Proxy = Just (messages, length messages)
    where
      messages = map snd $ toSamples (Proxy :: Proxy Message)

instance ToParam (QueryParam "offset" Int) where
  toParam Proxy = DocQueryParam
    "offset"
    ["0", "25", "-10"]
    "offset from beginning (positive) or from end (negative)"
    Normal

postMessage :: MVar [Message] -> Person -> Message -> EitherT ServantErr IO ()
postMessage mvar _p msg = liftIO $ modifyMVar_ mvar $ \ messages ->
  return (messages ++ [msg])

dataDir :: FilePath
dataDir = "chat/"

getMessages :: MVar [Message] -> Maybe Int -> EitherT ServantErr IO ([Message], Int)
getMessages mvar (fromMaybe 0 -> offset) = do
  messages <- liftIO $ readMVar mvar
  return $ (, length messages) $ if offset >= 0
    then drop offset messages
    else reverse $ take (negate offset) $ reverse messages

main :: IO ()
main = do
  mvar <- newMVar []
  let port = 8087
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ serve chatApi (chatApp mvar)
