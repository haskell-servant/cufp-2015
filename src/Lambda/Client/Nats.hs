
module Lambda.Client.Nats where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Lambda.Api
import           Lambda.Client.Basics hiding (main)

main :: IO ()
main = eitherT (error . show) return $ do
    result <- (add # nat 3) # nat 4
    liftIO $ putStrLn $ pretty result
    e <- eval result
    liftIO . putStrLn $ pretty e
    liftIO . print $ toNat e

zero :: M Term
zero = lam "s" (lam "z" (var "z"))

nat :: Integer -> M Term
nat n =
  lam "s" (lam "z" (inner n))
  where
    inner 0 = var "z"
    inner x | x > 0 = var "s" # inner (pred x)
    inner _ = liftIO $ error "nat: negative number"

-- \ a -> \ b -> \ s -> \ z -> (a s) (b s z)
add :: M Term
add = lam "a" (lam "b" (lam "s" (lam "z"
  ((var "a" # var "s") # ((var "b" # var "s") # var "z")))))