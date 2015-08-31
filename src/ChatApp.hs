module ChatApp where

import Data.Monoid
import ChatApi
import Servant
import System.IO
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

chatApp :: Server ChatApi
chatApp = postMessage

postMessage :: Person -> Message -> EitherT ServantErr IO ()
postMessage p (SimpleMessage msg) = liftIO $ appendFile (dataDir <> show p) msg

-- Like appendFile, but creates file if missing.
appendFile' :: FilePath -> String -> IO ()
appendFile' fp msg = openFile fp AppendMode >>= \h -> hPutStr h msg

dataDir :: FilePath
dataDir = "chat/"

main :: IO ()
main = run 8087 $ serve chatApi chatApp
