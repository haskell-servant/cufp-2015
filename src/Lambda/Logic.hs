-- The untyped lambda calculus. This will form the core business logic for our
-- application.
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

-- A pretty-printer that might help keep you sane.
pretty :: Term -> String
pretty (Var x) = x
pretty (Lambda x t) = "(\\" ++ x ++ " -> " ++ pretty t ++ ")"
pretty (App x t) = "(" ++ pretty x ++ ") " ++ pretty t

-- [TASK] Implement eval for our lambda calculus
evaluate :: Term -> Term
evaluate = error "not yet implemented"
