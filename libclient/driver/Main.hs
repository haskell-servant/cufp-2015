{-# LANGUAGE DeriveGeneric #-}

import           Control.Monad.Trans.Either
import qualified GHC.Generics
import           Servant.API
import           Servant.Client
import           System.Console.GetOpt.Generics
import           System.Exit

import           Client
import           CufpApi

data Options
  = Options {
    newHost :: Maybe String,
    newPort :: Maybe Int,
    serverHost :: String,
    serverPort :: Int
  }
  deriving (GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options

main :: IO ()
main = do
  options <- modifiedGetArguments $
    AddShortOption "serverHost" 'H' :
    AddShortOption "serverPort" 'p' :
    []
  case (newHost options, newPort options) of
    (Nothing, Nothing) -> do
      nodes <- try $ getNodes
      mapM_ print nodes
    (Just h, Just p) -> do
      try $ postNodesNew $ Node h p
    (Just _, Nothing) -> die "please add --new-port"
    (Nothing, Just _) -> die "please add --new-host"

try :: EitherT ServantError IO a -> IO a
try action = do
  r <- runEitherT action
  either (die . show) return r
