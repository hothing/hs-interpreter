module Interpret (
    Scalar,
    Context,
    createContext,
    getValue,
--    evalProg,
    evalExpr
  ) where
import Synt
import Data.Map as M

data Scalar = SInt Int | SReal Double
  deriving (Eq, Ord, Show)

data Context = Context {
    variables :: M.Map String Scalar
  }
  deriving (Eq, Show)

createContext :: Context
createContext = Context { variables = M.empty }

getValue :: Context -> String -> Maybe Scalar
getValue ctx name = M.lookup name $ variables ctx

--evalProg :: Synt.Program -> Either String Context
--evalProg (Program (ExprList lst)) = evalProg' createContext lst
-- TODO: applicative functor

evalExpr :: Context -> Synt.Expr -> Either String Context
evalExpr ctx (Expr vname rval) =
  case evalRVal ctx rval of
    Right value -> Right $ ctx { variables = M.insert vname value $ variables ctx }
    Left err -> Left err

evalRVal :: Context -> Synt.RVal -> Either String Scalar
evalRVal ctx rval = Right $ SInt 0
