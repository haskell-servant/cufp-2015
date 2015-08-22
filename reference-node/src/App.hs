
module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BS
import           Network.Wai
import           Servant
import           Servant.Docs

import           CufpApi

app :: IO Application
app = do
  mvar <- newMVar []
  return $ serve ipManager (server mvar)

server :: MVar [Node] -> Server IpManager
server mvar =
       (listIps mvar
  :<|> postIp mvar)
  :<|> apiDocs

-- | List all known nodes
listIps :: MVar [Node] -> EitherT ServantErr IO [Node]
listIps mvar = liftIO $ readMVar mvar

-- | Add a new node
postIp :: MVar [Node] -> Node -> EitherT ServantErr IO ()
postIp mvar ip =
  liftIO $ modifyMVar_ mvar $ \ ips -> return (ips ++ [ip])

-- | Get documentation describing the server API.
apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs ipManager'

persistNodes :: FilePath -> [Node] -> IO ()
persistNodes fp ns = BS.writeFile fp (encode ns)

loadNodes :: FilePath -> IO (Maybe [Node])
loadNodes fp = decode <$> BS.readFile fp
