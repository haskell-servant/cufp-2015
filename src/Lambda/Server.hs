{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.Server where

import           Control.Monad.Trans.Either
import           Data.String.Conversions
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
  :<|> return . cs . pretty

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

instance (ToSample a a, ToSample b b) => ToSample (a, b) (a, b) where
  toSample Proxy = (,) <$>
    (toSample (Proxy :: Proxy a)) <*>
    (toSample (Proxy :: Proxy b))

instance ToSample Term Term where
  toSample Proxy = Just (Lambda "x" (Var "x"))

instance ToSample ST ST where
  toSample Proxy = fmap (cs . pretty) $ toSample (Proxy :: Proxy Term)

instance ToCapture (Capture "parameter" String) where
  toCapture Proxy = DocCapture "parameter" "name of the parameter of the lambda abstraction"

instance ToCapture (Capture "var" String) where
  toCapture Proxy = DocCapture "var" "variable name"

runServer :: Port -> IO ()
runServer p = do
  let settings =
        setPort p $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show p)) $
        defaultSettings
  runSettings settings $ logStdoutDev $ serve lambdaApi lambdaServer
