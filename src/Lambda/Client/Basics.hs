
module Lambda.Client.Basics where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client
import           System.Exit

import           Lambda.Api

url :: BaseUrl
url = BaseUrl Http "jkarni.com" 8100

type M a = EitherT ServantError IO a

try :: M a -> IO a
try action = do
  r <- runEitherT action
  either (die . show) return r

-- * api functions

var :: String -> M Term
lambdaC :: String -> Term -> M Term
app :: (Term, Term) -> M Term
eval :: _
var :<|> lambdaC :<|> app :<|> eval = client lambdaApi url

lambda :: String -> M Term -> M Term
lambda p b = lambdaC p =<< b

(#) :: M Term -> M Term -> M Term
f # a = do
  ft <- f
  at <- a
  app (ft, at)

identity :: M Term
identity = lambda "x" (var "x")

main :: IO ()
main = do
  try $ do
    c <- (identity # identity)
    liftIO $ print c
    r <- eval c
    liftIO $ print r
