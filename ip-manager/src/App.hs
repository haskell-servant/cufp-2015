
module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Network.Wai
import           Servant

import           IpApi

app :: IO Application
app = do
  mvar <- newMVar []
  return $ serve ipManager (server mvar)

server :: MVar [Ip] -> Server IpManager
server mvar =
       listIps mvar
  :<|> postIp mvar

listIps :: MVar [Ip] -> EitherT ServantErr IO [Ip]
listIps mvar = liftIO $ readMVar mvar

postIp :: MVar [Ip] -> Ip -> EitherT ServantErr IO ()
postIp mvar ip =
  liftIO $ modifyMVar_ mvar $ \ ips -> return (ips ++ [ip])
