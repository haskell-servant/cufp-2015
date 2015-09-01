
module Lambda.Logic where

data Term = Var String
          | Lambda String Term
          | App (Term, Term)


data Scope

eval :: Scope -> Term -> Term
eval s (App (Lambda s t), t2)

{-fn = let   -}
    {-(c1 :<|> c2) =  client api url-}
