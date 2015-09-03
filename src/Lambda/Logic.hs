{-# LANGUAGE DeriveGeneric #-}

module Lambda.Logic where

import           Data.Aeson
import           GHC.Generics

data Term = Var String
          | Lambda String Term
          | App Term Term
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Term
instance FromJSON Term

evaluate :: Term -> Term
evaluate = _
