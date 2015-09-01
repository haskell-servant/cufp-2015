module Lambda.Logic where
import Data.Maybe (fromJust)
import qualified Data.Map as Map

data Term = Var String
          | Lambda String Term
          | App (Term, Term)
    deriving (Show, Eq)


type Env = Map.Map String Term

eval :: Env -> Term -> Term
eval env (App (Lambda s t, t2)) = let t2' = eval env t2 in eval (Map.insert s t2 env) t
eval env (App (_, _)) = error "applying to non-lambda"
eval env (Var x) = fromJust $ Map.lookup x env
eval env (Lambda s t) = Lambda s t
