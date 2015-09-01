{-# LANGUAGE DeriveGeneric #-}

module Lambda.Logic where

import           Control.Applicative
import           Data.Aeson
import           GHC.Generics

data Term = Var String
          | Lambda String Term
          | App Term Term
  deriving (Show, Read, Eq, Ord, Generic)

pretty :: Term -> String
pretty (Var x) = x
pretty (Lambda x t) = "(\\" ++ x ++ " -> " ++ pretty t ++ ")"
pretty (App x t) = "(" ++ pretty x ++ ") " ++ pretty t

toNat :: Term -> Maybe Integer
toNat (Lambda s (Lambda z e)) = inner e
  where
    inner x = case x of
      Var v | v == z -> Just 0
      Var _ -> Nothing
      App (Var v) r | v == s -> succ <$> inner r
      _ -> Nothing
toNat _ = Nothing

instance ToJSON Term
instance FromJSON Term

eval :: Term -> Term
eval x = case step x of
  Nothing -> x
  Just next -> eval next

step :: Term -> Maybe Term
step t = case t of
  Var _ -> Nothing
  Lambda v e -> Lambda v <$> step e
  App (Lambda v e) argument ->
    Just (replace v argument e)
  App f x ->
    (App <$> step f <*> pure x) <|>
    (App <$> pure f <*> step x)

replace :: String -> Term -> Term -> Term
replace needle argument e = case e of
  Var v | v == needle -> argument
  Var _ -> e
  Lambda v body | v == needle -> Lambda v body
  Lambda v body -> Lambda v (replace needle argument body)
  App f x -> App (replace needle argument f) (replace needle argument x)
