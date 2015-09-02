
module Chat.Client where

import Control.Monad
import           Control.Monad.Trans.Either
import           Servant.API
import           Control.Concurrent
import           Servant.Client

import           Chat.Api

postMessage :: Person -> Message -> EitherT ServantError IO ()
getMessages :: Maybe Int -> EitherT ServantError IO ([Message], Int)
_ :<|> postMessage :<|> getMessages =
  client chatApi (BaseUrl Http "localhost" 8087)

second :: Int
second = 100000

keepGettingMessages :: IO ()
keepGettingMessages = go 0
  where
    go n = runEitherT (getMessages $ Just n) >>= \c -> case c of
      Left err -> print err >> threadDelay second >> go n
      Right (msg, len) -> do
        when (length msg > 0) $ print msg
        threadDelay second >> go len

main :: Person -> IO ()
main p = forkIO keepGettingMessages >> forever (do
    msg <- getLine
    r <- runEitherT $ postMessage p (SimpleMessage msg)
    case r of
        Left e   -> putStrLn $ "error: " ++ show e
        Right () -> return ())
