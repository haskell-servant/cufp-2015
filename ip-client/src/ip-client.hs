{-# LANGUAGE DeriveGeneric #-}

import           Control.Monad.Trans.Either
import qualified GHC.Generics
import           Servant.API
import           Servant.Client
import           System.Console.GetOpt.Generics
import           System.Exit

import           IpApi

data Options
  = Options {
    newIp :: Maybe String,
    host :: String,
    port :: Int
  }
  deriving (GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options

main :: IO ()
main = do
  options <- modifiedGetArguments $
    AddShortOption "port" 'p' :
    AddShortOption "newIp" 'n' :
    []
  let url = BaseUrl Http (host options) (port options)
      (getListIps :<|> postNewIp) = client ipManager url
  case newIp options of
    Nothing -> do
      ips <- try $ getListIps
      mapM_ print ips
    Just new -> do
      ip <- maybe (die "invalid ip") return $ parseIp new
      try $ postNewIp ip

try :: EitherT ServantError IO a -> IO a
try action = do
  r <- runEitherT action
  either (die . show) return r
