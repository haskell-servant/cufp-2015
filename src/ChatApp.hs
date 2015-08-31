module ChatApp where

import Data.Monoid
import ChatApi
import Servant

chatApp :: Server ChatApi
chatApp =

postMessage :: Person -> Message -> EitherT ServantErr IO ()
postMessage p msg = appendFile (dataDir <> show p) msg

dataDir :: FilePath
dataDir = "chat/"
