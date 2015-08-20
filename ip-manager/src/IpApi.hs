{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module IpApi where

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           Data.String.Conversions
import           Data.Word
import           Servant
import           Text.Read (readMaybe)

type IpManager =
       "ips" :> Get '[JSON] [Ip]
  :<|> "new-ip" :> ReqBody '[JSON] Ip :> Post '[JSON] ()

ipManager :: Proxy IpManager
ipManager = Proxy

data Ip
  = Ip (Word8, Word8, Word8, Word8)
  deriving (Show, Read)

instance ToJSON Ip where
  toJSON ip =
    String $ cs $ showIp ip

instance FromJSON Ip where
  parseJSON v = case v of
    String s -> maybe err return (parseIp (cs s))
    _ -> err
    where
      err :: Monad m => m a
      err = fail (show v)

parseIp :: String -> Maybe Ip
parseIp s = case splitOn "." s of
  ws
    | length ws == 4
    -> do
      [a, b, c, d] <- mapM readMaybe ws
      return $ Ip (a, b, c, d)
  _ -> Nothing

showIp :: Ip -> String
showIp (Ip (a, b, c, d)) =
  intercalate "." $
  map show [a, b, c, d]
