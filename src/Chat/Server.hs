{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Chat.Server where

import           Servant
import           Network.Wai.Handler.Warp
import qualified System.Logging.Facade as Log
import           Control.Monad.Trans.Either

import           Chat.Api
import           MarkdownCT

chatApp :: Server ChatApi
chatApp = error "not yet implemented"

main :: IO ()
main = do
  let port = 8087
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ serve chatApi chatApp

runServer :: Port -> IO ()
runServer = error "not yet implemented"
