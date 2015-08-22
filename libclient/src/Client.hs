
module Client where

import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client

import           CufpApi

getNodes :: Host -> Port -> EitherT ServantError IO [Node]
getNodes h p = do
  let url = BaseUrl Http h p
      (getNodesG :<|> _) = client ipManager url
  getNodesG

postNodesNew :: Host -> Port -> Node -> EitherT ServantError IO ()
postNodesNew h p node = do
  let url = BaseUrl Http h p
      (_ :<|> postNodesNewG) = client ipManager url
  postNodesNewG node
