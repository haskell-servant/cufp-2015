
module Chat.Client where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client
import           System.Console.GetOpt.Generics
import qualified System.Logging.Facade as Log

import           Chat.Api

postMessage :: Person -> Message -> EitherT ServantError IO ()
postMessage = _

getMessages :: Maybe Int -> EitherT ServantError IO ([(Person, Message)], Int)
getMessages = _

second :: Int
second = 1000000

keepGettingMessages :: IO ()
keepGettingMessages = go 0
  where
    go n = runEitherT (getMessages $ Just n) >>= \c -> case c of
      Left err -> print err >> threadDelay second >> go n
      Right (msgs, len) -> do
        forM_ msgs $ \(Person sender, Message msg) -> do
          putStrLn (sender ++ " > " ++ msg)
        threadDelay second >> go len

main :: IO ()
main = simpleCLI $ \ n -> do
  _ <- forkIO keepGettingMessages >> forever (do
    msg <- getLine
    r <- runEitherT $ postMessage (Person n) (Message msg)
    case r of
      Left e   -> Log.error $ "error: " ++ show e
      Right () -> return ())
  return ()
