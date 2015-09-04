-- The untyped lambda calculus. This will form the core business logic for our
-- application.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Lambda.Logic where

import           Control.Applicative
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
evaluate t = case step t of
  Nothing -> t
  (Just t') -> evaluate t'

step :: Term -> Maybe Term
step = \ case
  Var _ -> Nothing
  Lambda param body ->
    Lambda param <$> step body
  App (Lambda param body) x ->
    Just $ replace param body x
  App f x ->
    (App <$> step f <*> pure x) <|>
    (App f <$> step x)

replace :: String -> Term -> Term -> Term
replace var body x = case body of
  Var v
    | v == var -> x
    | otherwise -> Var v
  Lambda p b
    | p == var -> Lambda p b
    | otherwise -> Lambda p (replace var b x)
  App f a -> App (replace var f x) (replace var a x)
