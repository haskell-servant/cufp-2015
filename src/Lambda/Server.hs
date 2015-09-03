{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.Server where

import           Control.Monad.Trans.Either
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Servant.Docs
import           System.Logging.Facade as Log

import           Lambda.Api
import           Lambda.Logic
import           MarkdownCT

lambdaServer :: Server LambdaApi
lambdaServer =
       apiDocs
  :<|> var
  :<|> lambda
  :<|> app
  :<|> eval'

var :: Monad m => String -> m Term
var = return . Var

lambda :: Monad m => String -> Term -> m Term
lambda s = return . Lambda s

app :: Monad m => (Term, Term) -> m Term
app (f, x) = return $ App f x

eval' :: Monad m => Term -> m Term
eval' t = return $ eval t

-- | Get documentation describing the server API.
apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs lambdaApi

runServer :: Port -> IO ()
runServer p = do
  let settings =
        setPort p $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show p)) $
        defaultSettings
  runSettings settings $ logStdoutDev $ serve lambdaApi lambdaServer
