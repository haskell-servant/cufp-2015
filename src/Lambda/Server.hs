module Lambda.Server where

import Lambda.Api
import Lambda.Logic
import Control.Monad.Trans.Either
import Servant

var :: String -> EitherT ServantErr IO Term
var = return . Var

lambda :: String -> Term -> EitherT ServantErr IO Term
lambda s = return . Lambda s

app :: (Term, Term) -> EitherT ServantErr IO Term
app = return . App
