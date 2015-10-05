{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits
import           System.Process

-- Apis

data Get a

data a :> b :: k -> * -> *
infixr :>

data QueryParam path typ

-- client

class HasClient api where
  type Client api :: *
  mkClient :: Proxy api -> String -> Client api

instance (Read a) => HasClient (Get a) where
  type Client (Get a) = IO a
  mkClient Proxy host = do
    let url = host
    print url
    read <$> readProcess "curl" [url, "-v", "-s"] ""

instance (KnownSymbol path, HasClient subApi) =>
  HasClient ((path :: Symbol) :> subApi) where
    type Client (path :> subApi) = Client subApi
    mkClient Proxy host = mkClient subProxy (host ++ "/" ++ path)
      where
        subProxy :: Proxy subApi
        subProxy = Proxy
        path :: String
        path = symbolVal (Proxy :: Proxy path)

instance (Show typ, KnownSymbol name, HasClient subApi) =>
  HasClient (QueryParam name typ :> subApi) where
    type Client (QueryParam name typ :> subApi) = typ -> Client subApi
    mkClient Proxy host = \ typ ->
      let url = host ++ "?" ++ name ++ "=" ++ show typ
          name = symbolVal (Proxy :: Proxy name)
          subApi = Proxy :: Proxy subApi
      in mkClient subApi url

-- example

type RedditApi = "posts" :> Get [String]

redditApi :: Proxy RedditApi
redditApi = Proxy

main :: IO ()
main = do
  output <- mkClient redditApi "http://localhost:8080"
  print output
