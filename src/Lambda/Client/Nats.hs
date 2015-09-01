
module Lambda.Client.Nats where

import           Control.Monad.IO.Class
import           System.Exit

import           Lambda.Api
import           Lambda.Client.Basics hiding (main)

main :: IO ()
main = do
  result <- try $ ((add # nat 3) # nat 4)
  putStrLn $ pretty result
  e <- try $ eval result
  putStrLn $ pretty e

zero :: M Term
zero = lambda "s" (lambda "z" (var "z"))

nat :: Integer -> M Term
nat n =
  lambda "s" (lambda "z" (inner n))
  where
    inner 0 = var "z"
    inner x | x > 0 = var "s" # inner (pred x)
    inner _ = liftIO $ die "nat: negative number"

-- \ a -> \ b -> \ s -> \ z -> (a s) (b s z)
add :: M Term
add = lambda "a" (lambda "b" (lambda "s" (lambda "z"
  ((var "a" # var "s") # ((var "b" # var "s") # var "z")))))
