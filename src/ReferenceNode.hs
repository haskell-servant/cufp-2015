
module ReferenceNode where

import           Network.Wai.Handler.Warp
import           System.Logging.Facade as Log
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.Set as Set
import           Network.Wai
import           Servant
import           Servant.Docs

import           CufpApi

run :: IO ()
run = do
  let serverPort = 8080
      settings =
        setPort serverPort $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show serverPort)) $
        defaultSettings
  runSettings settings =<< app

app :: IO Application
app = do
  mvar <- newMVar Set.empty
  return $ serve cufpApi (server mvar)

server :: MVar (Set.Set Node) -> Server CufpApi
server mvar =
       listIps mvar
  :<|> postIp mvar
  :<|> apiDocs
  :<|> deleteNode mvar

-- | List all known nodes
listIps :: MVar (Set.Set Node) -> EitherT ServantErr IO [Node]
listIps mvar = Set.toList <$> liftIO (readMVar mvar)

-- | Add a new node
postIp :: MVar (Set.Set Node) -> Node -> EitherT ServantErr IO ()
postIp mvar ip =
  liftIO $ modifyMVar_ mvar $ \ ips -> return (Set.insert ip ips)

deleteNode :: MVar (Set.Set Node) -> Node -> EitherT ServantErr IO ()
deleteNode mvar node =
  liftIO $ modifyMVar_ mvar $ \ ips -> return (Set.delete node ips)

-- | Get documentation describing the server API.
apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs cufpApi
