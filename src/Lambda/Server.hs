module Lambda.Server where

import Lambda.Api
import Lambda.Logic
import Control.Monad.Trans.Either
import Network.Wai.Handler.Warp
import Servant


lambdaServer :: Server LambdaApi
lambdaServer = var
          :<|> lambda
          :<|> app
          :<|> eval'

var :: String -> EitherT ServantErr IO Term
var = return . Var

lambda :: String -> Term -> EitherT ServantErr IO Term
lambda s = return . Lambda s

app :: (Term, Term) -> EitherT ServantErr IO Term
app (f, x) = return $ App f x

eval' :: Term -> EitherT ServantErr IO Term
eval' t = return $ eval t

runServer :: Port -> IO ()
runServer p = run p $ serve lambdaApi lambdaServer
