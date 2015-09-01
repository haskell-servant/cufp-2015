
module Main where

import           Network.Wai.Handler.Warp
import           Servant

import           Lambda.Api

main :: IO ()
main = run 8080 (serve lambdaApi server)

server :: Server LambdaApi
server =
       _
  :<|> _
