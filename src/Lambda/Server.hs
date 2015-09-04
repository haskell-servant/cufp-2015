module Lambda.Server where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           System.Logging.Facade as Log
import           Servant

import           Lambda.Api
import           Lambda.Logic

-- [TASK] Implement this server
lambdaServer :: Server LambdaApi
lambdaServer = Prelude.error "not yet implemented"

runServer :: Port -> IO ()
runServer p = do
  let settings =
        setPort p $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show p)) $
        defaultSettings
  runSettings settings $ logStdoutDev $ serve lambdaApi lambdaServer
