
module Lambda.Client.Basics where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client

import           Lambda.Api

url :: BaseUrl
url = BaseUrl Http "localhost" 9000

type M a = EitherT ServantError IO a

-- * api functions

var :: String -> M Term
lambda :: String -> Term -> M Term
app :: (Term, Term) -> M Term
eval :: Term -> M Term
_ :<|> var :<|> lambda :<|> app :<|> eval :<|> pretty = client lambdaApi url

lam :: String -> M Term -> M Term
lam p b = lambda p =<< b

(#) :: M Term -> M Term -> M Term
f # a = do
  ft <- f
  at <- a
  app (ft, at)

identity :: M Term
identity = lam "x" (var "x")

-- (\ x -> x x) (\ x -> x x)
infinite :: M Term
infinite =
  let t = lam "x" (var "x" # var "x")
  in t # t

main :: IO ()
main = eitherT (error . show) return $ do
  c <- (identity # identity)
  -- liftIO $ putStrLn $ pretty c
  r <- eval c
  return ()
  -- liftIO $ putStrLn $ pretty r
