{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Control.Monad
import           Data.Aeson
import           Data.List
import           Data.List.Split
import           Data.Maybe
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
  toJSON (Ip (a, b, c, d)) =
    String $ cs $
    intercalate "." $
    map show [a, b, c, d]

instance FromJSON Ip where
  parseJSON v = case v of
    String s -> case splitOn "." (cs s) of
      words
        | length words == 4
        -> maybe err return $ do
          [a, b, c, d] <- mapM readMaybe words
          return $ Ip (a, b, c, d)
      _ -> err
    _ -> err
    where
      err :: Monad m => m a
      err = fail (show v)
