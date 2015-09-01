
module Lambda.Client.Nats where

import           Control.Monad.Trans.Either
import           System.Exit

import           Lambda.Client.Basics
import           Lambda.Logic

main :: IO ()
main = do
  result <- try $ ((add # nat 3) # nat 4)
  print result

try :: M a -> IO a
try action = do
  r <- runEitherT action
  either (die . show) return r

zero :: M Term
zero = do
  zVar <- var "z"
  s <- lambda "z" zVar 
  lambda "s" s

nat :: Integer -> M Term
nat 0 = zero

add :: M Term
add = wrapNat _

wrapNat :: (Term -> Term -> M Term) -> M Term
wrapNat = _
