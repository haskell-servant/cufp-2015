
module Main where

import           Network.Wai.Handler.Warp
import           System.Logging.Facade as Log

import           App

main :: IO ()
main = do
  let port = 8080
      settings =
        setPort port $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< app
