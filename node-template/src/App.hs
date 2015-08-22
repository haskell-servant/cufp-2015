
module App where

import           Network.Wai
import           Servant

import           CufpApi

app :: IO Application
app = do
  return $ serve ipManager server

server :: Server IpManager
server = error "not yet implemented"
