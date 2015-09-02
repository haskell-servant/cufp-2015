
module Chat.Client where

import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client

import           Chat.Api

postMessage :: Person -> Message -> EitherT ServantError IO ()
getMessages :: Maybe Int -> EitherT ServantError IO ([Message], Int)
_ :<|> postMessage :<|> getMessages =
  client chatApi (BaseUrl Http "localhost" 8087)
