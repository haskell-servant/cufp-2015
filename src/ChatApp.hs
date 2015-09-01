{-# LANGUAGE OverloadedStrings #-}
module ChatApp where

import Data.Monoid
import ChatApi
import Servant
import Servant.Client
import System.IO
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

chatApp :: Server ChatApi
chatApp = postMessage

personFile :: Person -> FilePath
personFile (Person p) = show p <> ".chat"

postMessage :: Person -> Message -> EitherT ServantErr IO ()
postMessage p (SimpleMessage msg) = liftIO $ appendFile (dataDir <> personFile p) msg

-- Like appendFile, but creates file if missing.
appendFile' :: FilePath -> String -> IO ()
appendFile' fp msg = openFile fp AppendMode >>= \h -> hPutStr h msg

dataDir :: FilePath
dataDir = "chat/"

postMessage' :: Person -> Message -> EitherT ServantError IO ()
postMessage' = client chatApi (BaseUrl Http "localhost" 8087)

main :: IO ()
main = do
    run 8087 $ serve chatApi chatApp
