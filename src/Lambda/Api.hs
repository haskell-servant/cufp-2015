{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Api (
  module Lambda.Api,
  Term,
 ) where

import           Data.Proxy
import           Data.Text
import           Servant.API

import           Lambda.Logic
import           MarkdownCT

type LambdaApi =
       Docs

  :<|> Var
  :<|> Lambda
  :<|> App
  :<|> Eval

  :<|> Pretty

lambdaApi :: Proxy LambdaApi
lambdaApi = Proxy

type Docs = "docs" :> Get '[Markdown] Markdown

type Var = "var" :> Capture "var" String :> Get '[JSON] Term

type Lambda = "lambda" :> Capture "parameter" String :> ReqBody '[JSON] Term :> Get '[JSON] Term

type App = "app" :> ReqBody '[JSON] (Term, Term) :> Get '[JSON] Term

type Eval = "eval" :> ReqBody '[JSON] Term :> Get '[JSON] Term

type Pretty = "convenience" :> "pretty" :> ReqBody '[JSON] Term :> Get '[PlainText] Text 
