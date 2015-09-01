{-# LANGUAGE DeriveGeneric #-}

module Lambda.Logic where

import           Data.Aeson
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           GHC.Generics

data Term = Var String
          | Lambda String Term
          | App (Term, Term)
  deriving (Show, Read, Eq, Ord, Generic)

pretty :: Term -> String
pretty (Var x) = x
pretty (Lambda x t) = "(\\" ++ x ++ " -> " ++ pretty t ++ ")"
pretty (App (x,t)) = pretty x ++ " " ++ pretty t

instance ToJSON Term
instance FromJSON Term


type Env = Map.Map String Term

eval :: Env -> Term -> Term
eval env (App (Lambda s t, t2)) = let t2' = eval env t2 in eval (Map.insert s t2' env) t
eval env (App (x, t)) = case eval env x of
    l@(Lambda _ _) -> eval env (App (l, t))
    _ -> error $ pretty x
eval env (Var x) = fromJust $ Map.lookup x env
eval _ (Lambda s t) = Lambda s t
