{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Api where

import           Data.Proxy
import           Servant.API

import           Lambda.Logic

type LambdaApi =
       Var
  :<|> Lambda
  :<|> App
  :<|> Eval

lambdaApi :: Proxy LambdaApi
lambdaApi = Proxy

type Var = "var" :> Capture "var" String :> Get '[JSON] Term

type Lambda = "lambda" :> Capture "parameter" String :> ReqBody '[JSON] Term :> Get '[JSON] Term

type App = "app" :> ReqBody '[JSON] (Term, Term) :> Get '[JSON] Term

type Eval = "eval" :> ReqBody '[JSON] Term :> Get '[JSON] Term
