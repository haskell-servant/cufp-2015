
module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson (encode, decode)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BS
import           Network.Wai
import           Servant
import           Servant.Docs

import           CufpApi

app :: IO Application
app = do
  mvar <- newMVar Set.empty
  return $ serve ipManager (server mvar)

server :: MVar (Set.Set Node) -> Server IpManager
server mvar =
       listIps mvar
  :<|> postIp mvar
  :<|> apiDocs

-- | List all known nodes
listIps :: MVar (Set.Set Node) -> EitherT ServantErr IO [Node]
listIps mvar = Set.toList <$> liftIO (readMVar mvar)

-- | Add a new node
postIp :: MVar (Set.Set Node) -> Node -> EitherT ServantErr IO ()
postIp mvar ip =
  liftIO $ modifyMVar_ mvar $ \ ips -> return (Set.insert ip ips)

-- | Get documentation describing the server API.
apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs ipManager

persistNodes :: FilePath -> [Node] -> IO ()
persistNodes fp ns = BS.writeFile fp (encode ns)

loadNodes :: FilePath -> IO (Maybe [Node])
loadNodes fp = decode <$> BS.readFile fp
