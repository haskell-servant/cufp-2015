{-# LANGUAGE OverloadedStrings #-}

import           Data.String.Conversions
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
  let settings =
        setPort 8080 $
        setBeforeMainLoop (putStrLn "started.") $
        defaultSettings
  runSettings settings $ \ request respond -> do
    print $ queryString request
    respond $ case pathInfo request of
      [] -> send "root"
      ["posts"] -> send ["foo", "bar"]
      _ -> responseLBS notFound404 [] "Not Found"

send :: Show a => a -> Response
send a = responseLBS ok200 [] $ cs (show a)
