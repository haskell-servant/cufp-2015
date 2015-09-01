
module Lambda.Client.Basics where

import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client

import           Lambda.Api
import           Lambda.Logic

type M a = EitherT ServantError IO a

var :: String -> M Term
lambda :: String -> Term -> M Term
var :<|> lambda :<|> _ = client lambdaApi url 

url :: BaseUrl
url = BaseUrl Http "jkarni.com" 9000

(#) :: M Term -> M Term -> M Term
f # a = _
