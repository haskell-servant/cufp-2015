{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Api (
  module Lambda.Api,
  Term,
  pretty,
  toNat,
 ) where

import           Data.Proxy
import           Servant.API

import           Lambda.Logic
import           MarkdownCT

type LambdaApi =
       Docs

  :<|> Var
  :<|> Lambda
  :<|> App
  :<|> Eval

lambdaApi :: Proxy LambdaApi
lambdaApi = Proxy

type Var = "var" :> Capture "var" String :> Get '[JSON] Term

type Lambda = "lambda" :> Capture "parameter" String :> ReqBody '[JSON] Term :> Get '[JSON] Term

type App = "app" :> ReqBody '[JSON] (Term, Term) :> Get '[JSON] Term

type Eval = "eval" :> ReqBody '[JSON] Term :> Get '[JSON] Term

type Docs = "docs" :> Get '[Markdown] Markdown
