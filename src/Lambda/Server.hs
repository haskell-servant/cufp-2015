module Lambda.Server where

import Lambda.Api
import Lambda.Logic
import Network.Wai.Handler.Warp
import Servant


lambdaServer :: Server LambdaApi
lambdaServer = var
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

runServer :: Port -> IO ()
runServer p = run p $ serve lambdaApi lambdaServer
